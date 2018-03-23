## ** stevedore_object support **
stevedore_object <- function(env, class) {
  api_version <- env$.parent$.api_client$api_version
  env$help <- function(help_type = getOption("help_type")) {
    stevedore_object_help(class, api_version, help_type) # nocov
  }


  for (nm in names(env)) {
    x <- env[[nm]]
    if (is.function(x) && !inherits(x, "docker_client_method")) {
      env[[nm]] <- docker_client_method_nonapi(x, class, nm)
    }
  }

  class(env) <- c(class, "stevedore_object")
  lock_environment(env)
  env
}


new_stevedore_object <- function(parent) {
  ret <- new_empty_env()
  ret$.parent <- parent %||% ret
  ret
}


docker_client_method_nonapi <- function(fun, class, name) {
  help <- docker_api_client_help(class, name)
  if (is.null(help$summary)) {
    stop(sprintf("missing help for %s$%s", class, name))
  } else if (!setequal(names(help$args), names(formals(fun)))) {
    stop(sprintf("incorrect help for %s$%s", class, name))
  }

  attr(fun, "help") <- help
  attr(fun, "name") <- name
  class(fun) <- "docker_client_method"
  fun
}


## NOTE: - this is not always utils::help here because I need to make
## sure that we can go via devtool's help in testing.  So I am trying
## a bit of a hack here, using a conditional in the NAMESPACE - this
## is similar to the approach taken in backports
##
## NOTE: help_type is passed through by option because the devtools
## shim does not pass help_type through, but the option manages to
## make it.
##
## NOTE: This is not testable because while one can turn off the pager
## (see test-help.R for the approach) it all goes pear shaped in covr
## and returns no file at all!  And then testing with mockr goes very
## badly with the devtools help shim.  So this looks untestable at
## present.
stevedore_object_help <- function(name, api_version, help_type) {
  ## nocov start
  oo <- options(stevedore.help.api_version = api_version,
                help_type = help_type)
  on.exit(options(oo))
  help(name, package = "stevedore")
  ## nocov end
}


subset_stevedore_object <- function(x, name) {
  .subset2(x, name) %||%
    stop(sprintf("No element '%s' within '%s' object", name, class(x)[[1]]),
         call. = FALSE)
}


##' @export
`$.stevedore_object` <- function(x, name) {
  subset_stevedore_object(x, name)
}


##' @export
`[[.stevedore_object` <- function(x, i, ...) {
  assert_scalar_character(i)
  subset_stevedore_object(x, i)
}


report_warnings <- function(x, action) {
  n <- length(x)
  if (n > 0L) {
    warning(sprintf(
      "%d %s produced while %s:\n%s",
      n, ngettext(n, "warning", "warnings"), action,
      paste0("- ", x, collapse = "\n")),
      call. = FALSE, immediate. = TRUE)
  }
}

## ** printers **
pull_status_printer <- function(stream = stdout()) {
  if (is.null(stream)) {
    return(function(x) {})
  }
  assert_is(stream, "connection")
  last_is_progress <- FALSE
  width <- getOption("width")
  endl <- if (isatty(stream)) "" else "\n"

  function(x) {
    if (last_is_progress) {
      reset_line(stream, width)
    }
    status <- x$status
    if (length(x$progressDetail > 0L)) {
      last_is_progress <<- TRUE
      cur <- x$progressDetail[["current"]]
      tot <- x$progressDetail[["total"]]
      str <- sprintf("%s: %s %s/%s %d%%%s", x[["id"]], x[["status"]],
                     pretty_bytes(cur), pretty_bytes(tot),
                     round(cur / tot * 100),
                     endl)
    } else {
      last_is_progress <<- FALSE
      if (!is.null(x$error)) {
        ## TODO: there's also errorDetail$message here too
        ##
        ## (See definitions/BuildInfo in the spec yaml)
        str <- paste0(x$error, "\n")
      } else if (!is.null(x$status) && is.null(x$id)) {
        str <- paste0(x$status, "\n")
      } else if (!is.null(x$status) && !is.null(x$id)) {
        str <- sprintf("%s %s\n", x$status, x$id)
      } else {
        str <- ""
      }
    }

    cat(str, file = stream, sep = "")
  }
}


build_status_printer <- function(stream = stdout()) {
  print_output <- !is.null(stream)
  if (print_output) {
    assert_is(stream, "connection")
  }
  function(x) {
    if (print_output && "stream" %in% names(x)) {
      cat(x$stream, file = stream, sep = "")
    }
  }
}


build_status_id <- function(content) {
  lines <- strsplit(raw_to_char(content), "\r\n")[[1]]
  dat <- lapply(lines, from_json)
  err <- vlapply(dat, function(x) "error" %in% names(x))
  if (any(err)) {
    stop(build_error(dat[[which(err)[[1]]]]$error))
  }

  ## This is the regular expression used in the python package (but
  ## with a newline following, which I have made optional here).
  re <- "(^Successfully built |sha256:)([0-9a-f]+)\n?$"
  dat <- lapply(lines, from_json)
  is_id <- vlapply(dat, function(el)
    "stream" %in% names(el) && grepl(re, el$stream))
  if (!any(is_id)) {
    stop("Could not determine created image id") # nocov [stevedore bug]
  }
  sub(re, "\\2", dat[[max(which(is_id))]]$stream)
}


after_system_login <- function(response, params, self) {
  serveraddress <- from_json(params$body)$serveraddress
  self$.parent$.api_client$auth$set(serveraddress, params$body)
  invisible(TRUE)
}


after_container_list <- function(response, ...) {
  ## TODO: I'm not really sure of the situation where we get more
  ## than one name here; there might be a better way of dealing with
  ## this.  One option would be to refuse to treat this as a list
  ## column unless explicitly asked for, returning generally the
  ## first element.  But I don't know how reasonable that is.
  response$names[] <- lapply(response$names, drop_leading_slash)
  response$name <- vcapply(response$names, function(x)
    if (length(x) > 0) x[[1]] else NA_character_)
  response
}


after_container_create <- function(response, params, self) {
  report_warnings(response$warnings, "creating container")
  docker_client_container(response$id, self$.parent)
}


after_container_archive <- function(response, params, self) {
  if (is.null(params$dest)) {
    response
  } else {
    writeBin(response, params$dest)
    invisible(params$dest)
  }
}


after_exec_create <- function(response, params, self) {
  docker_client_exec(response$id, self$.parent)
}


after_container_logs <- function(response, params, ...) {
  if (isTRUE(params$query$follow)) {
    invisible(response$content_handler(response$response$content))
  } else {
    response
  }
}


after_container_path_stat <- function(response, ...) {
  from_json(base64decode(response$docker_container_path_stat))
}


after_container_top <- function(response, ...) {
  m <- matrix(unlist(response$processes),
              byrow = TRUE, nrow = length(response$processes))
  colnames(m) <- response$titles
  ## NOTE: some of these can be non-text.  Not sure how to safely do
  ## that though.  So for now it's all going to be character.
  as.data.frame(m, stringsAsFactors = FALSE)
}


after_image_commit <- function(response, params, self) {
  docker_client_image(response$id, self$.parent)
}


after_image_build <- function(response, params, self) {
  id <- build_status_id(response$response$content)
  docker_client_image(id, self$.parent)
}


after_image_pull <- function(response, params, self) {
  id <- sprintf("%s:%s", params$query$fromImage, params$query$tag)
  docker_client_image(id, self$.parent)
}


after_image_push <- function(response, ...) {
  lines <- strsplit(raw_to_char(response$response$content), "\r\n")[[1]]
  last <- from_json(lines[[length(lines)]])
  ## Oddly, for 1.29 at least, I don't see an error from the API,
  ## just in here:
  if ("error" %in% names(last)) {
    stop(push_error(last$error))
  }
  invisible(TRUE)
}


after_network_create <- function(response, params, self) {
  docker_client_network(response$id, self$.parent)
}


after_volume_create <- function(response, params, self) {
  docker_client_volume(response$name, self$.parent)
}


after_volume_list <- function(response, ...) {
  report_warnings(response$warnings, "reading volume list")
  response$volumes
}


after_exec_start <- function(response, ...) {
  ## TODO: this also wants to catch an input argument (which does
  ## not yet exist) that controls if output is to be returned.  The
  ## argument will be 'collect' or something.  Alternatively we
  ## might return 'self' and implement some sort of async on top of
  ## this (I think Jeroen has written all the required bits into
  ## curl).
  invisible(decode_chunked_string(response$response$content))
}


after_container_update <- function(response, params, self) {
  report_warnings(response$warnings, "updating container")
  invisible(self)
}


after_service_create <- function(response, params, self) {
  ret <- docker_client_service(response$id, self$.parent)
  if (!params$detach) {
    docker_client_service_wait_converged(
      ret, params$timeout, time_wait_stable = params$time_wait_stable,
      stream = params$stream)
  }
  ret
}


after_task_logs <- function(response, params, ...) {
  if (isTRUE(params$query$follow)) {
    invisible(response$content_handler(response$response$content))
  } else {
    response
  }
}


after_secret_create <- function(response, ...) {
  response$id
}


after_secret_list <- function(response, ...) {
  response$name <- vcapply(response$spec, function(x) x$name)
  ord <- c("id", "name")
  response[c(ord, setdiff(names(response), ord))]
}


invisible_self <- function(response, params, self) {
  invisible(self)
}


docker_stream_printer <- function(stream, style = "auto") {
  if (is.null(stream)) {
    return(function(x) {})
  }
  assert_is(stream, "connection")
  function(x) {
    if (inherits(x, "docker_stream")) {
      x <- format(x, style = style, dest = stream)
      cat(x, file = stream, sep = "")
    } else {
      writeLines(x, stream)
    }
  }
}


streaming_text <- function(callback) {
  assert_function(callback)
  res <- raw()
  ret <- function(x) {
    res <<- c(res, x)
    callback(decode_chunked_string(x))
  }
  attr(ret, "content") <- function() res
  ret
}


streaming_json <- function(callback) {
  assert_function(callback)
  res <- raw()
  ret <- function(x) {
    res <<- c(res, x)
    lapply(strsplit(raw_to_char(x), "\r\n")[[1]],
           function(line) callback(from_json(line)))
    invisible()
  }
  attr(ret, "content") <- function() res
  ret
}


## ** validators **
validate_command <- function(x) {
  if (length(x) == 1L && inherits(x, "AsIs")) {
    x <- split_command(x)
  }
  x
}


## character: open a file in mode wb and ensure closing on exit
## logical: suppress stream or log to stdoud (FALSE, TRUE)
## NULL: no stream
## connection object: stream to open connection
validate_stream <- function(stream, mode = "wb",
                            name = deparse(substitute(stream))) {
  close <- FALSE
  if (is.character(stream)) {
    close <- TRUE
    stream <- file(stream, mode)
  } else if (is.null(stream) || identical(stream, FALSE)) {
    stream <- NULL
  } else if (isTRUE(stream)) {
    stream <- stdout()
  } else {
    assert_is(stream, "connection", name = name)
  }
  list(stream = stream, close = close)
}


validate_tar_directory <- function(path, dockerfile,
                                   name = deparse(substitute(path))) {
  if (is.character(path)) {
    assert_directory(path, name = name)
    path <- build_tar(path, dockerfile)
  } else {
    assert_raw(path, name = name)
  }
  path
}


## TODO: For starters, let's use the string format only.  Later we'll
## come back and allow more interesting approaches that use volume
## mappings in a more abstract way.  This function will return the two
## bits that we need - half for create and half for host_config.
validate_volumes <- function(volumes) {
  if (is.null(volumes) || length(volumes) == 0L) {
    return(NULL)
  }
  assert_character(volumes)

  binds <- volumes
  re_ro <- ":ro$"
  is_ro <- grepl(re_ro, volumes)
  if (any(is_ro)) {
    volumes[is_ro] <- sub(re_ro, "", volumes[is_ro])
  }
  re <- "^(.+):([^:]+)$"
  ok <- grepl(re, volumes)
  if (any(!ok)) {
    stop(sprintf("Volume mapping %s does not not match '<src>:<dest>[:ro]",
                 paste(squote(volumes[!ok]), collapse = ", ")))
  }
  list(binds = binds,
       volumes = set_names(rep(list(NULL), length(volumes)),
                           sub(re, "\\1", volumes)))
}


validate_ports <- function(ports) {
  if (is.null(ports) || length(ports) == 0L) {
    return(NULL)
  }
  if (is.logical(ports) && length(ports) == 1L &&
      identical(as.vector(ports), TRUE)) {
    return(TRUE)
  }
  if (is_integer_like(ports)) {
    ports <- as.character(ports)
  }
  assert_character(ports)

  ## NOTE: this is _not_ enough to capture what docker can do but it's
  ## a starting point for working out to complete support.
  re_random <- "^[0-9]+$"
  re_explicit <- "^([0-9]+):([0-9]+)$"

  i_random <- grepl(re_random, ports)
  i_explicit <- grepl(re_explicit, ports)

  ok <- i_random | i_explicit
  if (any(!ok)) {
    ## TODO: This does not include all possibilities
    stop(sprintf("Port binding %s does not not match '<host>:<container>'",
                 paste(squote(ports[!ok]), collapse = ", ")))
  }

  n <- length(ports)
  protocol <- rep("tcp", n)
  host_ip <- character(n)
  host_port <- character(n)
  container_port <- character(n)

  container_port[i_random] <- ports[i_random]
  container_port[i_explicit] <- sub(re_explicit, "\\2", ports[i_explicit])

  host_port[i_explicit] <- sub(re_explicit, "\\1", ports[i_explicit])

  container_port <- sprintf("%s/%s", container_port, protocol)

  ## TODO: this bit with the unboxing should move into HostConfig
  ## validation at the same time that the case binding is done there.
  ## Or, because here we're explicitly modifying the object perhaps
  ## this is OK?
  build_binding <- function(ip, port) {
    list(list(HostIp = jsonlite::unbox(ip),
              HostPort = jsonlite::unbox(port)))
  }
  port_bindings <- set_names(Map(build_binding, host_ip, host_port),
                             container_port)
  list(port_bindings = port_bindings,
       ports = set_names(rep(list(NULL), length(ports)), container_port))
}


## TODO: pass names through here too I think
validate_image_and_tag <- function(image, tag = NULL,
                                   name_image = deparse(substitute(image)),
                                   name_tag = deparse(substitute(tag))) {
  dat <- parse_image_name(image)
  if (is.null(dat$tag)) {
    dat$tag <- tag %||% "latest"
  } else {
    if (!is.null(tag)) {
      stop(sprintf("If '%s' includes a tag, then '%s' must be NULL",
                   name_image, name_tag))
    }
  }
  dat
}


validate_tar_input <- function(input, name = deparse(substitute(input))) {
  if (!is.raw(input)) {
    input <- tar_file(input)
  }
  input
}


validate_env <- function(env, name = deparse(substitute(env))) {
  if (length(env) == 0L) {
    return(NULL)
  }
  if (is.recursive(env)) {
    if (any(lengths(env) > 1)) {
      stop(sprintf("All elements of '%s' must be scalar (or use atomic vector)",
                   name, call. = FALSE))
    }
    env <- vcapply(env, function(x) if (is.null(x)) "" else as.character(x))
  }

  ## It could be possible to allow environment variables to be passed
  ## through as named pairs ("A=B") but not done as named on the
  ## vector, letting these through wherever names are NULL/"", but I
  ## don't think it's worth the complication.
  assert_named(env, TRUE, name = name)
  value <- unname(env)
  value[is.na(value)] <- ""
  sprintf("%s=%s", names(env), value)
}


validate_secret_data <- function(data) {
  if (!is.raw(data)) {
    assert_scalar_character(data, what = "a scalar character or raw")
  }
  base64encode(data, TRUE)
}


## This needs major work:
##
## - proper interface for remapping to different files (e.g. secret:dest)
## - UID/GID/Mode handling (though defaults here will generally be ok)
## - this will need a lot of tweaking if the docker API changes with
##   versions, because this depends on the api structure .
validate_service_secrets <- function(task_template, client) {
  if (length(task_template$ContainerSpec$Secrets) == 0L) {
    task_template$ContainerSpec$Secrets <- NULL
    return(task_template)
  }

  given <- task_template$ContainerSpec$Secrets
  assert_character(given)
  known <- client$secrets$list()

  id <- name <- rep(NA_character_, length(given))

  ## This feels really awkward!
  i <- given %in% known$id
  id[i] <- given[i]
  name[i] <- known$name[match(given[i], known$id)]
  i <- given %in% known$name & is.na(id)
  name[i] <- given[i]
  id[i] <- known$id[match(given[i], known$name)]

  if (any(is.na(id))) {
    err <- given[is.na(id)]
    stop(sprintf("Unknown %s: %s",
                 ngettext(length(err), "secret", "secrets"),
                 paste(squote(err), collapse = ", ")))
  }

  f <- function(id, name) {
    list(SecretID = jsonlite::unbox(id),
         SecretName = jsonlite::unbox(name),
         File = list(Name = jsonlite::unbox(name),
                     UID = jsonlite::unbox("0"),
                     GID = jsonlite::unbox("0"),
                     Mode = jsonlite::unbox(292L))) # 292 -> 444 in oct
  }
  task_template$ContainerSpec$Secrets <- unname(Map(f, id, name))

  task_template
}


validate_service_replicas <- function(replicas, global) {
  global <- isTRUE(assert_scalar_logical(global))
  if (is.null(replicas) && !global) { # default:
    return(NULL)
  }

  if (global && !is.null(replicas)) {
    stop("Cannot use 'replicas' with 'global'")
  }
  if (global) {
    mode <- list(Global = NULL)
  } else {
    replicas <- assert_scalar_integer(replicas)
    mode <- list(Replicated = list(Replicas = jsonlite::unbox(replicas)))
  }

  mode
}


## ** utilities **
get_image_id <- function(x, name = deparse(substitute(x))) {
  if (inherits(x, "docker_image")) {
    x$id()
  } else {
    ## TODO: error message should allow for docker_image alternative
    assert_scalar_character(x, name)
    x
  }
}

get_network_id <- function(x, name = deparse(substitute(x))) {
  if (inherits(x, "docker_network")) {
    x$id()
  } else {
    ## TODO: error message should allow for docker_network alternative
    assert_scalar_character(x, name)
    x
  }
}


as_docker_filter <- function(x, name = deparse(substitute(x))) {
  if (length(x) == 0L) {
    NULL
  } else if (inherits(x, "json")) {
    x
  } else {
    assert_named(x, TRUE, name)
    if (!(is.character(x) || (is.list(x) && all(vlapply(x, is.character))))) {
      stop(sprintf(
        "'%s' must be a character vector or list of character vectors",
        name))
    }
    jsonlite::toJSON(as.list(x))
  }
}


## ** macros **
mcr_prepare_stream_and_close <- function(name, mode = "wb") {
  substitute({
    stream_data <- validate_stream(name, mode)
    stream <- stream_data$stream
    if (stream_data$close) {
      on.exit(close(stream), add = TRUE)
    }
  }, list(name = name, mode = mode))
}


mcr_volumes_for_create <- function(volumes, host_config) {
  substitute({
    volumes <- validate_volumes(volumes)
    if (!is.null(volumes)) {
      ## TODO: consider checking that host_config$Binds is not given here
      host_config$Binds <- volumes[["binds"]]
      volumes <- volumes[["volumes"]]
    }
  }, list(volumes = volumes, host_config = host_config))
}


mcr_ports_for_create <- function(ports, host_config) {
  substitute({
    ports <- validate_ports(ports)
    if (!is.null(ports)) {
      if (identical(ports, TRUE)) {
        host_config$PublishAllPorts <- jsonlite::unbox(TRUE)
        ports <- NULL
      } else {
        ## TODO: consider checking that host_config$PortBindings is
        ## not given here
        host_config$PortBindings <- ports[["port_bindings"]]
        ports <- ports[["ports"]]
      }
    }
  }, list(ports = ports, host_config = host_config))
}


mcr_network_for_create <- function(network, host_config) {
  substitute({
    if (!is.null(network)) {
      network <- get_network_id(network)
      host_config$NetworkMode <- jsonlite::unbox(network)
      network <- list(network = NULL)
    }
  }, list(network = network, host_config = host_config))
}


mcr_process_image_and_tag <- function(image, tag) {
  substitute({
    image_tag <- validate_image_and_tag(image, tag)
    image <- image_tag[["image"]]
    tag <- image_tag[["tag"]]
  }, list(image = image, tag = tag))
}


mcr_prepare_auth <- function(image, registry_auth) {
  substitute(
    registry_auth <- api_client$auth$get(parse_image_name(image)$registry),
    list(image = image, registry_auth = registry_auth))
}


mcr_prepare_push <- function(name, tag, registry_auth) {
  substitute({
    name <- parse_image_name(name)
    tag <- name$tag %||% "latest"
    registry_auth <- api_client$auth$get(name$registry) %||% base64encode("{}")
    name <- sprintf("%s/%s", name$registry, name$image)
  }, list(name = name, tag = tag, registry_auth = registry_auth))
}


## Testing help
dummy_id <- function() {
  .stevedore$dummy_id %||% HELP
}


is_dummy_id <- function(id) {
  identical(id, dummy_id())
}


## TODO: make this more like add_inspect?
docker_client_getter <- function(getter, parent, name = "id") {
  env <- new.env(parent = baseenv())
  env$getter <- getter
  env$parent <- parent

  args <- alist(id =)
  names(args) <- name
  body <- substitute(getter(id, parent), list(id = as.name(name)))

  as.function(c(args, body), env)
}


docker_client_add_inspect <- function(id, key_name, inspect_name, self,
                                      key_name_in = key_name) {
  if (is_dummy_id(id)) {
    inspect <- function(id) set_names(list(id), key_name)
  } else {
    inspect <- docker_client_method(inspect_name, self)
  }

  self$.attrs <- inspect(id)

  self$reload <- function() {
    self$.attrs <- inspect(self$.attrs[[key_name]])
    invisible(self)
  }

  self$inspect <- function(reload = TRUE) {
    if (reload) {
      self$reload()
    }
    self$.attrs
  }

  self[[key_name]] <- function() self$.attrs[[key_name]]

  set_names(list(self$.attrs[[key_name]]), key_name_in)
}


docker_client_container_ports <- function(attrs) {
  ports <- attrs$network_settings$ports

  if (length(ports) == 0L) {
    container_port <- protocol <- host_ip <- host_port <- character(0)
  } else {
    container <- strsplit(names(ports), "/", fixed = TRUE)
    stopifnot(all(lengths(container) == 2L))
    len <- viapply(ports, nrow)
    container_port <- rep(vcapply(container, "[[", 1L), len)
    protocol <- rep(vcapply(container, "[[", 2L), len)
    host_ip <- unlist(lapply(ports, "[[", "host_ip"), use.names = FALSE)
    host_port <- unlist(lapply(ports, "[[", "host_port"), use.names = FALSE)
  }
  data_frame(container_port, protocol, host_ip, host_port)
}


docker_client_container_image <- function(self) {
  attrs <- self$inspect(FALSE)
  image_id <- sub("^(sha256:)", "", attrs$image)
  docker_client_image(image_id, self$.parent)
}


## TODO: repo and tag should be separate as for tag (with option
## to do them together).
docker_client_image_untag <- function(repo_tag, image) {
  repo_tag <- image_name_with_tag(repo_tag)
  valid <- setdiff(image$inspect()$repo_tags, "<none>:<none>")
  if (!(repo_tag %in% valid)) {
    stop(sprintf("Invalid repo_tag '%s' for image '%s'",
                 repo_tag, image$id()))
  }
  image$.parent$images$remove(repo_tag, noprune = TRUE)
  image$reload()
}


docker_client_image_tags <- function(attrs) {
  setdiff(attrs$repo_tags, "<none>:<none>")
}


docker_client_network_containers <- function(reload, self) {
  containers <- self$inspect(reload)$containers
  lapply(names(containers), docker_client_container, self$.parent)
}


docker_client_volume_map <- function(attrs, path, readonly = FALSE) {
  assert_scalar_character(path)
  assert_scalar_logical(readonly)
  fmt <- "%s:%s"
  if (readonly) {
    fmt <- paste0(fmt, ":ro")
  }
  sprintf(fmt, attrs$name, path)
}


docker_client_service_tasks <- function(self, filters) {
  if (length(filters) == 0L) {
    filters <- character(0)
  } else if ("service" %in% names(filters)) {
    stop("'service' is not a valid filter name for this method")
  }
  filters[["service"]] <- self$id()
  tasks <- self$.parent$tasks$list(filters)

  ret <- lapply(tasks$id, function(id)
    tryCatch(self$.parent$tasks$get(id), error = function(e) NULL))
  ret[!vlapply(ret, is.null)]
}


docker_client_service_ps <- function(self, resolve_names, filters) {
  tasks <- self$tasks(filters = filters)

  d <- lapply(tasks, function(t) t$inspect(FALSE))

  task_id <- substr(vcapply(d, "[[", "id"), 1L, 12L)
  slot <- viapply(d, "[[", "slot")
  image <- vcapply(d, function(x) x$spec$container_spec$image)

  desired_state <- vcapply(d, "[[", "desired_state")
  current_state <- vcapply(d, function(x) x$status$state)
  when <- time_ago(vcapply(d, function(x) x$status$timestamp))
  node_id <- vcapply(d, "[[", "node_id")

  if (resolve_names) {
    nodes <- self$.parent$nodes$list()
    node_name <-
      vcapply(nodes$description, "[[", "hostname")[match(node_id, nodes$id)]
    task_prefix <- self$name(FALSE)
  } else {
    node_name <- node_id
    task_prefix <- self$id()
  }
  task_name <- sprintf("%s.%d", task_prefix, slot)

  ## TODO: error and ports are not done yet
  ret <- data_frame(
    id = task_id,
    name = task_name,
    image = image,
    node = node_name,
    desired_state = desired_state,
    current_state = current_state,
    when = when)
  ret <- ret[order(slot), ]
  rownames(ret) <- NULL
  ret
}


docker_client_service_wait_converged <- function(service, timeout,
                                                 t0 = Sys.time(),
                                                 time_poll = 0.1,
                                                 time_wait_stable = 5,
                                                 stream = stdout()) {
  ## TODO: report that tasks are erroring
  n <- service$inspect(FALSE)$spec$mode$replicated$replicas
  pr <- make_service_start_progress(stream)

  message(sprintf("Waiting for %d %s for %s (%s) to start",
                  n, ngettext(n, "task", "tasks"),
                  service$name(FALSE), service$id()))
  t1 <- t0 + timeout
  repeat {
    tasks <- service$tasks()
    state <- vcapply(tasks, function(t) t$inspect(FALSE)$status$state)
    m <- pr(state)
    if (sum(state == "running") == n) {
      cat2("\n", file = stream)
      break
    }
    if (Sys.time() > t1) {
      cat2("\n", file = stream)
      stop("service has not converged in time (but docker is still trying)",
           call. = FALSE)
    }
    Sys.sleep(time_poll)
  }

  tasks <- tasks[state == "running"]
  message(sprintf("Wating %s seconds for service to ensure convergence",
                  time_wait_stable))
  t1 <- Sys.time() + time_wait_stable
  while (Sys.time() < t1) {
    cat2(".", file = stream)
    ok <- vcapply(tasks, function(t)
      tryCatch(t$state(), error = function(e) "gone")) == "running"
    if (!all(ok)) {
      cat2("\nTask has failed, trying again\n", file = stream)
      docker_client_service_wait_converged(service, timeout, t0,
                                           time_poll, time_wait_stable,
                                           stream)
    }
    Sys.sleep(time_poll)
  }
  cat2("done\n", file = stream)
}


make_service_start_progress <- function(stream) {
  states_active <- c("new" = "new",
                   "allocated" = "alloc",
                   "pending" = "pend",
                   "assigned" = "assign",
                   "accepted" = "accept",
                   "preparing" = "prep",
                   "ready" = "ready",
                   "starting" = "start",
                   "running" = "running")
  states_end <- c("complete", "shutdown", "rejected", "failed")
  title <- paste0(paste(states_active, collapse = " > "), "\n")
  pos_end <- cumsum(unname(nchar(states_active)) + 3L) - 3L

  if (is.null(stream)) {
    return(function(state) NULL)
  }

  last <- NULL
  function(state) {
    i <- na.omit(match(state, names(states_active)))
    n <- tabulate(i, length(states_active))
    np <- pos_end[seq_along(n)[n > 0L]]
    nn <- n[n > 0L]

    progress <- ""
    for (j in seq_along(nn)) {
      ns <- as.character(nn[[j]])
      len_bar <- np[[j]] - nchar(progress) - nchar(ns) - 1L
      char <- if (nchar(progress) == 0) "=" else "-"
      progress <- paste0(progress, strrep(char, len_bar), ">", ns)
    }

    if (any(state == "failed")) {
      pad <- strrep(" ", nchar(title) - nchar(progress) + 1L)
      progress <- paste0(progress, pad, "w/ fails")
    }

    if (is.null(last)) {
      cat(title, file = stream)
    } else {
      reset_line(stream, nchar(last))
    }
    cat(progress, file = stream)
    last <<- progress
  }
}


pass_through <- function(x) {
  x
}
