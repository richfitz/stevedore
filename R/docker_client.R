##' Create a docker client object
##' @title Create docker client
##' @param ... Reserved for future use
##'
##' @param api_version Version of the API request from the api.
##'   Options are \code{NULL} (the default) - use the package's
##'   default version (currently 1.29), a version as a string or
##'   \code{\link{numeric_version}} object (supported between 1.25 and
##'   1.33), or the string \code{detect} which will use the highest
##'   version out of the version reported by the api and 1.33
##'
##' @export
docker_client <- function(..., api_version = NULL) {
  ## The design through here mimics the Python docker library; we have
  ## a concept of a "foo collection" (e.g., a container collection)
  ## that produces instances of "foo" objects (e.g., containers).
  ## This will be replicated for networks, volumes, etc.  Unlike the
  ## Python inteface we're not doing this with any fancy inheritance
  ## etc.
  cl <- docker_client_base(..., api_version = api_version)

  ret <- stevedore_object(
    "docker_client",
    events = docker_endpoint("system_events", cl),
    df = docker_endpoint("system_df", cl),
    info = docker_endpoint("system_info", cl),
    login = docker_endpoint("system_auth", cl),
    ping = docker_endpoint("system_ping", cl),
    version = docker_endpoint("system_version", cl),
    api_version = function() cl$http_client$api_version,
    lock = FALSE)

  ret$containers <- docker_client_container_collection(cl = cl, parent = ret)
  ret$images <- docker_client_image_collection(cl = cl, parent = ret)
  ret$networks <- docker_client_network_collection(cl = cl, parent = ret)
  ret$volumes <- docker_client_volume_collection(cl = cl, parent = ret)

  lock_environment(ret)
  ret
}

docker_client_container_collection <- function(..., cl, parent) {
  get_container <- function(id) {
    docker_client_container(id, cl)
  }
  after_create <- function(dat, ...) {
    report_warnings(dat$warnings)
    get_container(dat$id)
  }
  after_list <- function(dat, ...) {
    ## TODO: I'm not really sure of the situation where we get more
    ## than one name here; there might be a better way of dealing with
    ## this.  One option would be to refuse to treat this as a list
    ## column unless explicitly asked for, returning generally the
    ## first element.  But I don't know how reasonable that is.
    dat$names[] <- lapply(dat$names, drop_leading_slash)
    dat$name <- vcapply(dat$names, function(x)
      if (length(x) > 0) x[[1]] else NA_character_)
    dat
  }

  stevedore_object(
    "docker_container_collection",
    run = make_docker_run(parent),
    create = docker_endpoint(
      "container_create", cl,
      promote = c("image", "cmd"),
      rename = c(ports = "exposed_ports", network = "networking_config"),
      process = list(
        image = quote(image <- get_image_id(image)),
        cmd = quote(cmd <- check_command(cmd)),
        volumes = volumes_for_create(quote(volumes), quote(host_config)),
        ports = ports_for_create(quote(ports), quote(host_config)),
        network = network_for_create(quote(network), quote(host_config))),
      after = after_create),
    get = get_container,
    list = docker_endpoint(
      "container_list", cl,
      process = list(filters = validate_filter("filters")),
      after = after_list),
    remove = docker_endpoint(
      "container_delete", cl,
      process = list(id = quote(id <- get_image_id(id)))),
    prune = docker_endpoint("container_prune", cl))
}

docker_client_container <- function(id, client) {
  container_inspect <- docker_endpoint("container_inspect", client)
  attrs <- container_inspect(id)
  id <- attrs$id
  self <- NULL

  reload <- function() {
    attrs <<- container_inspect(id)
    invisible(self)
  }
  after_get_archive <- function(x, params) {
    if (is.null(params$dest)) {
      x
    } else {
      writeBin(x, params$dest)
      invisible(params$dest)
    }
  }
  after_exec <- function(x, ...) {
    ret <- docker_client_exec(x$id, client)
    ret
  }
  after_logs <- function(x, params) {
    if (isTRUE(params$query$follow)) {
      invisible(x$content_handler(x$response$content))
    } else {
      x
    }
  }
  after_path_stat <- function(x, ...) {
    from_json(rawToChar(openssl::base64_decode(x$docker_container_path_stat)))
  }
  after_top <- function(x, ...) {
    m <- matrix(unlist(x$processes), byrow = TRUE, nrow = length(x$processes))
    colnames(m) <- x$titles
    ## TODO: some of these can be non-text.  Not sure how to safely do
    ## that though.  So for now it's all going to be character.
    as.data.frame(m, stringsAsFactors = FALSE)
  }
  after_update <- function(x, ...) {
    report_warnings(x$warnings, "updating container")
    invisible(self)
  }
  ports <- function(reload = TRUE) {
    ports <- self$inspect(reload)$network_settings$ports
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
  fix_id <- list(id = id)

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  self <- stevedore_object(
    "docker_container",
    id = function() id,
    name = function() drop_leading_slash(attrs$name),
    image = function() {
      docker_client_image(
        strsplit(attrs$image, ":", fixed = TRUE)[[1L]][[2L]], client)
    },
    labels = function() attrs$config$labels,
    status = function() attrs$state$status,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    ## TODO: this one is hard because it might need to hijack the connection
    ## attach = docker_endpoint("container_attach", client, fix = fix_id)
    commit = docker_endpoint("image_commit", client,
                             fix = list(name = attrs$name)),
    diff = docker_endpoint("container_changes", client, fix = fix_id),
    ## TODO: inject 'start' into here too, which then requires passing
    ## detach through as well and dealing with those through the
    ## 'after' function.
    exec = docker_endpoint(
      "exec_create", client, fix = fix_id,
      rename = c(stdout = "attach_stdout", stderr = "attach_stderr",
                 stdin = "attach_stdin"),
      defaults = alist(stdout = TRUE, stderr = TRUE, cmd =),
      promote = "cmd",
      process = list(cmd = quote(cmd <- check_command(cmd))),
      after = after_exec),
    export = docker_endpoint("container_export", client, fix = fix_id),
    path_stat = docker_endpoint("container_path_stat", client, fix = fix_id,
                                after = after_path_stat),
    get_archive = docker_endpoint(
      "container_archive", client, fix = fix_id, extra = alist(dest =),
      process = list(dest = quote(assert_scalar_character_or_null(dest))),
      after = after_get_archive),
    put_archive = docker_endpoint("container_import", client, fix = fix_id),
    kill = docker_endpoint("container_kill", client, fix = fix_id),
    ## Logs; quite complicated in the case of 'follow'
    ## -  stream has an effect *only* if follow is TRUE
    logs = docker_endpoint(
      "container_logs", client, fix = fix_id,
      defaults = list(stdout = TRUE, stderr = TRUE),
      process = list(
        tail = quote(if (is.numeric(tail)) tail <- as.character(tail)),
        stream = validate_stream_and_close(quote(stream))),
      extra = alist(stream = stdout()),
      hijack = quote(if (isTRUE(follow))
                       streaming_text(exec_output_printer(stream))),
      after = after_logs),
    pause = docker_endpoint("container_pause", client, fix = fix_id),
    ## This should invalidate our container afterwards
    remove = docker_endpoint("container_delete", client, fix = fix_id),
    ## This might force refresh?
    rename = docker_endpoint("container_rename", client, fix = fix_id),
    resize = docker_endpoint("container_resize", client, fix = fix_id),
    restart = docker_endpoint("container_restart", client, fix = fix_id),
    start = docker_endpoint("container_start", client, fix = fix_id),
    ## TODO: expose stream (but with nice printing and escape instructions?)
    stats = docker_endpoint("container_stats", client,
                            fix = c(fix_id, stream = FALSE)),
    stop = docker_endpoint("container_stop", client, fix = fix_id),
    top = docker_endpoint("container_top", client, fix = fix_id,
                          after = after_top),
    unpause = docker_endpoint("container_unpause", client, fix = fix_id),
    update = docker_endpoint("container_update", client, fix = fix_id,
                             after = after_update),
    wait = docker_endpoint("container_wait", client, fix = fix_id),
    ports = ports,
    reload = reload)
  self
}

docker_client_image_collection <- function(..., cl, parent) {
  get_image <- function(id) {
    docker_client_image(id, cl)
  }
  after_build <- function(x, ...) {
    lines <- strsplit(raw_to_char(x$response$content), "\r\n")[[1]]
    ## This is the regular expression used in the python package (but
    ## with a newline following, which I have made optional here).
    re <- "(^Successfully built |sha256:)([0-9a-f]+)\n?$"
    dat <- lapply(lines, from_json)
    is_id <- vlapply(dat, function(el)
      "stream" %in% names(el) && grepl(re, el$stream))
    if (!any(is_id)) {
      stop("Could not determine created image id") # nocov [stevedore bug]
    }
    id <- sub(re, "\\2", dat[[max(which(is_id))]]$stream)
    get_image(id)
  }
  after_pull <- function(x, params) {
    ## TODO: check with the python version, but if called as
    ## pull("foo", tag = NULL) then *all* tags are pulled.  I don't
    ## think that is clever because I think this should only ever pull
    ## one image.
    get_image(params$query$fromImage)
  }
  stevedore_object(
    "docker_image_collection",
    ## TODO: control returning output too
    ## TODO: support multiple tags (accept vector and translate into
    ##   multiple 't' parameters - needs support in generated handlers
    build = docker_endpoint(
      "image_build", cl,
      rename = c(context = "input_stream", tag = "t"),
      extra = alist(stream = stdout()),
      process = list(stream = validate_stream_and_close(quote(stream)),
                     context = validate_tar_directory(quote(context))),
      hijack = quote(streaming_json(build_status_printer(stream))),
      after = after_build),
    get = get_image,
    list = docker_endpoint("image_list", cl),
    import = docker_endpoint("image_import", cl),
    ## TODO: need to deal with registry auth properly.  For now I'm just
    ##   eliminating it from the list.
    ## TODO: remove tag argument and fix to inject latest in if needed
    pull = docker_endpoint(
      "image_create", cl, rename = c("name" = "from_image"),
      drop = c("input_image", "from_src", "repo", "registry_auth"),
      defaults = alist(name =),
      hijack = quote(streaming_json(pull_status_printer(stdout()))),
      after = after_pull),
    push = docker_endpoint("image_push", cl),
    search = docker_endpoint("image_search", cl),
    remove = docker_endpoint("image_delete", cl),
    prune = docker_endpoint("image_prune", cl))
}

docker_client_image <- function(id, client) {
  image_inspect <- docker_endpoint("image_inspect", client)
  attrs <- image_inspect(id)
  name <- id
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- image_inspect(id)
    invisible(self)
  }
  fix_id_as_name = list(name = id)
  self <- stevedore_object(
    "docker_image",
    id = function() attrs$id,
    name = function() name,
    labels = function() attrs$config$labels,
    short_id = function() short_id(attrs$id),
    tags = function() setdiff(attrs$repo_tags, "<none>:<none>"),
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    history = docker_endpoint("image_history", client, fix = fix_id_as_name),
    ## TODO: this needs to add a 'filename' option for saving
    export = docker_endpoint("image_tarball", client, fix = fix_id_as_name),
    tag = docker_endpoint("image_tag", client, fix = fix_id_as_name),
    ## TODO: this would best be done with a wrapper around the
    ## incoming argument for 'repo_tag' but with the core function
    ## passed through docker_endpoint so that we can pass around
    ## defaults.  We'll be doing that with other functions later too.
    untag = function(repo_tag) {
      repo_tag <- image_name(repo_tag)
      image_delete <- docker_endpoint("image_delete", client)
      valid <- setdiff(image_inspect(id)$repo_tags, "<none>:<none>")
      if (!(repo_tag %in% valid)) {
        stop(sprintf("Invalid repo_tag '%s' for image '%s'",
                     repo_tag, attrs$id))
      }
      image_delete(repo_tag, noprune = TRUE)
    },
    ## NOTE: this removes by *id* which will not always work without a
    ## force - the name is not preserved on the way through this
    ## function.  Doing that might make more sense perhaps?
    remove = docker_endpoint("image_delete", client, fix = fix_id_as_name),
    reload = reload)
  self
}

docker_client_network_collection <- function(..., cl, parent) {
  get_network <- function(id) {
    docker_client_network(id, cl)
  }
  after_create <- function(dat, ...) {
    get_network(dat$id)
  }
  stevedore_object(
    "docker_network_collection",
    create = docker_endpoint("network_create", cl, after = after_create),
    get = get_network,
    list = docker_endpoint("network_list", cl),
    remove = docker_endpoint("network_delete", cl),
    prune = docker_endpoint("network_prune", cl))
}

docker_client_network <- function(id, client) {
  network_inspect <- docker_endpoint("network_inspect", client)
  attrs <- network_inspect(id)
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- network_inspect(id)
    invisible(self)
  }
  containers <- function(reload = TRUE) {
    containers <- self$inspect(reload)$containers
    lapply(names(containers), docker_client_container, client)
  }
  fix_id <- list(id = id)

  self <- stevedore_object(
    "docker_network",
    id = function() id,
    name = function() attrs$name,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    containers = containers,
    connect = docker_endpoint("network_connect", client, fix = fix_id),
    disconnect = docker_endpoint("network_disconnect", client, fix = fix_id),
    remove = docker_endpoint("network_delete", client, fix = fix_id),
    reload = reload)
  self
}

docker_client_volume_collection <- function(..., cl, parent) {
  get_volume <- function(id) {
    docker_client_volume(id, cl)
  }
  after_create <- function(dat, ...) {
    get_volume(dat$name)
  }
  after_list <- function(dat, ...) {
    report_warnings(dat$warnings, "reading volume list")
    dat$volumes
  }
  stevedore_object(
    "docker_volume_collection",
    create = docker_endpoint("volume_create", cl, after = after_create),
    get = get_volume,
    list = docker_endpoint("volume_list", cl, after = after_list),
    remove = docker_endpoint("volume_delete", cl),
    prune = docker_endpoint("volume_prune", cl))
}

docker_client_volume <- function(id, client) {
  volume_inspect <- docker_endpoint("volume_inspect", client)
  attrs <- volume_inspect(id)
  name <- attrs$name
  self <- NULL
  reload <- function() {
    attrs <<- volume_inspect(name)
    invisible(self)
  }

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  self <- stevedore_object(
    "docker_volume",
    name = function() attrs$name,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    remove = docker_endpoint("volume_delete", client, fix = list(name = name)),
    reload = reload)
  self
}

docker_client_exec <- function(id, client) {
  exec_inspect <- docker_endpoint("exec_inspect", client)
  attrs <- exec_inspect(id)
  self <- NULL
  reload <- function() {
    attrs <<- exec_inspect(id)
    invisible(self)
  }
  after_start <- function(x, params) {
    ## TODO: this also wants to catch an input argument (which does
    ## not yet exist) that controls if output is to be returned.  The
    ## argument will be 'collect' or something.  Alternatively we
    ## might return 'self' and implement some sort of async on top of
    ## this (I think Jeroen has written all the required bits into
    ## curl).
    invisible(decode_chunked_string(x$response$content))
  }
  ## Even though it feels like there *should* be a way, there is no
  ## way to get back to a detached exec instance.
  ## https://github.com/moby/moby/issues/9527
  self <- stevedore_object(
    "docker_exec",
    id = function() id,
    ## TODO: control stream (location etc) following the same problem
    ## in build.
    ## TODO: explicitly set 'detach' argument
    start = docker_endpoint(
      "exec_start", client, fix = list(id = id),
      extra = alist(stream = stdout()),
      hijack = quote(streaming_text(exec_output_printer(stream))),
      process = list(stream = validate_stream_and_close(quote(stream))),
      after = after_start),
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    resize = docker_endpoint("exec_resize", client, fix = list(id = id)),
    reload = reload)
  self
}

## TODO: The bits below here could do with some organisation

stevedore_object <- function(class, ..., lock = TRUE) {
  els <- list(...)
  assert_named(els, TRUE, "stevedore_object elements")
  ret <- list2env(els, parent = emptyenv())
  class(ret) <- c(class, "stevedore_object")
  if (lock) {
    lock_environment(ret)
  }
  ret
}

short_id <- function(x) {
  end <- if (string_starts_with(x, "sha256:")) 17L else 10L
  substr(x, 1, end)
}

drop_leading_slash <- function(x) {
  sub("^/", "", x)
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
                     prettyunits::pretty_bytes(cur),
                     prettyunits::pretty_bytes(tot),
                     round(cur / tot * 100),
                     endl)
    } else {
      last_is_progress <<- FALSE
      if (!is.null(x$error)) {
        ## TODO: there's also errorDetail$message here too
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

build_error <- function(message) {
  ret <- list(message = message, call = NULL)
  class(ret) <- c("build_error", "error", "condition")
  ret
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

## TODO: this is not just exec - also logs
## TODO: arrange for 'style' to be passed through here
exec_output_printer <- function(stream, style = "auto") {
  if (is.null(stream)) {
    return(function(x) {})
  }
  assert_is(stream, "connection")
  function(x) {
    x <- format(x, style = style, dest = stream)
    cat(x, file = stream, sep = "")
  }
}

##' @export
print.stevedore_object <- function(x, ..., indent = 2L) {
  nms <- sort(names(x))
  is_fn <- vlapply(nms, function(el) is.function(x[[el]]))

  cat(sprintf("<%s>\n", class(x)[[1]]))
  cat(sprintf("%s%s: %s\n", strrep(" " , indent), nms[!is_fn],
              vcapply(nms[!is_fn], function(el) class(x[[el]])[[1]])),
      sep = "")
  defns <- vcapply(nms[is_fn], function(el) capture_args(x[[el]], el, indent),
                   USE.NAMES = FALSE)
  cat(paste0(defns, "\n", collapse = ""))
  invisible(x)
}

validate_stream_and_close <- function(name, mode = "wb") {
  substitute(expression({
    if (is.character(name)) {
      name <- file(name, mode)
      on.exit(close(name), add = TRUE)
    } else if (!is.null(name)) {
      assert_is(name, "connection")
    }
  }), list(name = name, mode = mode))[[2]][[2]]
}

## TODO: see comments in tar_directory about setting this up for curl
## streaming from disk
## if (stream) {
##   name <- curl_stream_file(tar_directory(name)) # sets attr
##   on.exit(file.remove(name))
## } else {
##   tar_directory(name)
## }
validate_tar_directory <- function(name, stream = FALSE) {
  substitute(expression({
    if (is.character(name)) {
      assert_directory(name)
      name <- tar_directory(name)
    } else {
      assert_raw(name)
    }
  }), list(name = name))[[2]][[2]]
}

## NOTE: if this is not used anywhere else it might be better to do
## this inlining?
get_image_id <- function(x, name = deparse(substitute(x))) {
  if (inherits(x, "docker_image")) {
    x$id()
  } else {
    assert_scalar_character(x, name)
    x
  }
}

image_name <- function(x, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  if (!grepl(":", x, fixed = TRUE)) {
    x <- paste0(x, ":latest")
  }
  x
}

validate_filter <- function(name) {
  substitute(name <- as_docker_filter(name),
             list(name = as.name(name)))
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

docker_get_image <- function(image, client, name = deparse(substitute(image))) {
  if (inherits(image, "docker_image")) {
    image
  } else {
    image <- image_name(image, name)
    tryCatch(
      client$images$get(image),
      docker_error = function(e) {
        if (is_docker_error_not_found(e)) {
          message(sprintf("Unable to find image '%s' locally", image))
          client$images$pull(image)
        } else {
          stop(e)
        }
      })
  }
}

make_docker_run <- function(client) {
  force(client)
  ## TODO: this should pick up all the args from create rather than
  ## using dots.
  function(image, cmd = NULL, ..., detach = FALSE, rm = FALSE,
           stream = NULL) {
    if (rm && detach) {
      ## This is supported in API 1.25 and up - which agrees with our
      ## API support.
      ##
      ## What I we *do* need to do though is intercept any host_config
      ## argument here and modify rather than replacing it.
      ##
      ## NOTE: Must use PascalCase here because this is directly
      ## passed though - at the moment!  If I get this fixed up that
      ## might need to change (the manual unboxing would also not be
      ## needed).
      host_config <- list(AutoRemove = jsonlite::unbox(TRUE))
    } else {
      host_config <- NULL
    }
    image <- docker_get_image(image, client)
    container <- client$containers$create(image, cmd, ...,
                                          host_config = host_config)
    if (rm && !detach) {
      on.exit(container$remove(), add = TRUE)
    }
    container$start()
    if (detach) {
      return(container)
    }

    ## TODO: here, and possibly elsewhere, some simple rules about
    ## handling stream as an argument - TRUE/FALSE, etc.
    out <- container$logs(stream = stream, follow = TRUE)
    exit_status <- container$wait()$status_code

    if (rm) {
      container$inspect(TRUE)
    }
    if (exit_status != 0L) {
      stop(container_error(container, exit_status, cmd, image, out))
    }
    list(container = container, logs = out)
  }
}

container_error <- function(container, exit_status, cmd, image, out) {
  err <- out[attr(out, "stream") == "stderr"]
  if (length(err) > 0L) {
    err <- paste0("\n", err, collapse = "")
  } else {
    err <- ""
  }
  msg <- sprintf(
    "Command '%s' in image '%s' returned non-zero exit status %s%s",
    cmd, image$name(), exit_status, err)
  ret <- list(container = container, exit_status = exit_status,
              cmd = cmd, image = image, out = out, message = msg)
  class(ret) <- c("container_error", "docker_error", "error", "condition")
  ret
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

## TODO: consider a prefix for all the macro functions.
volumes_for_create <- function(volumes, host_config) {
  substitute(expression({
    volumes <- validate_volumes(volumes)
    if (!is.null(volumes)) {
      ## TODO: consider checking that host_config$Binds is not given here
      host_config$Binds <- volumes[["binds"]]
      volumes <- volumes[["volumes"]]
    }
  }), list(volumes = volumes, host_config = host_config))[[2]]
}

validate_ports <- function(ports) {
  if (is.null(ports) || length(ports) == 0L) {
    return(NULL)
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

ports_for_create <- function(ports, host_config) {
  substitute({
    ports <- validate_ports(ports)
    if (!is.null(ports)) {
      ## TODO: consider checking that host_config$PortBindings is not given here
      host_config$PortBindings <- ports[["port_bindings"]]
      ports <- ports[["ports"]]
    }
  }, list(ports = ports, host_config = host_config))
}

network_for_create <- function(network, host_config) {
  substitute({
    if (!is.null(network)) {
      assert_scalar_character(network)
      host_config$NetworkMode <- jsonlite::unbox(network)
      network <- list(network = NULL)
    }
  }, list(network = network, host_config = host_config))
}
