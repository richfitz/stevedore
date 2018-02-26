## ** stevedore_object support **
stevedore_object <- function(class, api_client, ..., lock = TRUE) {
  els <- list(...)
  assert_named(els, TRUE, "stevedore_object elements")

  els$help <- function(help_type = getOption("help_type")) {
    stevedore_object_help(class, api_client$api_version, help_type) # nocov
  }

  add_help_nonapi <- function(nm) {
    x <- els[[nm]]
    if (is.function(x) && !inherits(x, "docker_client_method")) {
      x <- docker_client_method_nonapi(x, class, nm)
    }
    x
  }
  els[] <- lapply(names(els), add_help_nonapi)

  ret <- list2env(els, parent = emptyenv())
  class(ret) <- c(class, "stevedore_object")
  if (lock) {
    lock_environment(ret)
  }
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
    if ("error" %in% names(x)) {
      stop(build_error(x$error))
    }
    if (print_output && "stream" %in% names(x)) {
      cat(x$stream, file = stream, sep = "")
    }
  }
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


mrc_prepare_auth <- function(auth, image, registry_auth) {
  substitute(
    registry_auth <- auth$get(parse_image_name(image)$registry),
    list(auth = auth, image = image, registry_auth = registry_auth))
}
