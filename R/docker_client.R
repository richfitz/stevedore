##' Create a docker client object
##' @title Create docker client
##' @param ... Reserved for future use
##'
##' @param api_version Version of the API request from the api.
##'   Options are \code{NULL} (the default) - use the package's
##'   default version (currently 1.29), a version as a string or
##'   \code{\link{numeric_version}} object (supported between 1.25 and
##'   1.32), or the string \code{detect} which will use the highest
##'   version out of the version reported by the api and 1.32
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

  containers <- docker_client_container_collection(cl = cl)
  images <- docker_client_image_collection(cl = cl)
  networks <- docker_client_network_collection(cl = cl)
  volumes <- docker_client_volume_collection(cl = cl)

  stevedore_object(
    "docker_client",
    containers = containers,
    images = images,
    networks = networks,
    volumes = volumes,
    events = docker_endpoint("system_events", cl),
    df = docker_endpoint("system_df", cl),
    info = docker_endpoint("system_info", cl),
    login = docker_endpoint("system_auth", cl),
    ping = docker_endpoint("system_ping", cl),
    version = docker_endpoint("system_version", cl),
    api_version = function() cl$http_client$api_version)
}

docker_client_container_collection <- function(..., cl) {
  get_container <- function(id) {
    docker_client_container(id, cl)
  }
  after_create <- function(dat) {
    report_warnings(dat$warnings)
    get_container(dat$id)
  }
  after_list <- function(dat) {
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
    ## run = ... - this one is complex (TODO)
    create = docker_endpoint("container_create", cl, after = after_create),
    get = get_container,
    list = docker_endpoint("container_list", cl, after = after_list),
    remove = docker_endpoint("container_delete", cl),
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
  after_exec <- function(x) {
    ## TODO: eventually we'll be passing start etc through here and
    ## doing things with them.
    ret <- docker_client_exec(x$id, client)
    ret
  }
  after_path_stat <- function(x) {
    from_json(rawToChar(openssl::base64_decode(x$docker_container_path_stat)))
  }
  after_top <- function(x) {
    m <- matrix(unlist(x$processes), byrow = TRUE, nrow = length(x$processes))
    colnames(m) <- x$titles
    ## TODO: some of these can be non-text.  Not sure how to safely do
    ## that though.  So for now it's all going to be character.
    as.data.frame(m, stringsAsFactors = FALSE)
  }
  after_update <- function(x) {
    report_warnings(x$warnings, "updating container")
    invisible(self)
  }
  fix_id <- list(id = id)

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  ##
  ## TODO: the vast bulk of this can be done more nicely with a simple
  ## list of functions.  That will plug into an eventual help system.
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
    ## attach = docker_endpoint("container_attach", client, fix_id)
    commit = docker_endpoint("image_commit", client, list(name = attrs$name)),
    diff = docker_endpoint("container_changes", client, fix_id),
    ## TODO: inject 'start' into here too, which then requires passing
    ## detach through as well.
    ##
    ## TODO: set stdout and stderr to TRUE by default
    exec = docker_endpoint("exec_create", client, fix_id, after_exec),
    export = docker_endpoint("container_export", client, fix_id),
    path_stat = docker_endpoint("container_path_stat", client, fix_id,
                                after_path_stat),
    get_archive = docker_endpoint("container_archive", client, fix_id),
    put_archive = docker_endpoint("container_import", client, fix_id),
    kill = docker_endpoint("container_kill", client, fix_id),
    ## TODO: stdout/stderr args become TRUE by default?
    logs = docker_endpoint("container_logs", client, fix_id),
    pause = docker_endpoint("container_pause", client, fix_id),
    ## This should invalidate our container afterwards
    remove = docker_endpoint("container_delete", client, fix_id),
    ## This might force refresh?
    rename = docker_endpoint("container_rename", client, fix_id),
    resize = docker_endpoint("container_resize", client, fix_id),
    restart = docker_endpoint("container_restart", client, fix_id),
    start = docker_endpoint("container_start", client, fix_id),
    ## TODO: expose stream (but with nice printing?)
    stats = docker_endpoint("container_stats", client,
                            c(fix_id, stream = FALSE)),
    stop = docker_endpoint("container_stop", client, fix_id),
    top = docker_endpoint("container_top", client, fix_id, after_top),
    unpause = docker_endpoint("container_unpause", client, fix_id),
    update = docker_endpoint("container_update", client, fix_id, after_update),
    wait = docker_endpoint("container_wait", client, fix_id),
    reload = reload)
  self
}

docker_client_image_collection <- function(..., cl) {
  get_image <- function(id) {
    docker_client_image(id, cl)
  }
  ## TODO: this is no good; we want to construct a *new* streamer each
  ## time with the result of 'stream'.  And then close that out if
  ## it's a file.
  after_build <- function(x) {
    lines <- strsplit(raw_to_char(x$response$content), "\r\n")[[1]]
    ## This is the regular expression used in the python package (but
    ## with a newline following, which I have made optional here).
    re <- "(^Successfully built |sha256:)([0-9a-f]+)\n?$"
    dat <- lapply(lines, from_json)
    is_id <- vlapply(dat, function(el)
      "stream" %in% names(el) && grepl(re, el$stream))
    if (!any(is_id)) {
      stop("Could not determine created image id")
    }
    id <- sub(re, "\\2", dat[[max(which(is_id))]]$stream)
    get_image(id)
  }
  after_pull <- function(x) {
    ## TODO: need to work out a pattern here for passing back
    ## arguments.  Otherwise this is just fairly nasty.
    repository <- sub(".*fromImage=([^?&]+).*", "\\1", x$response$url)
    if (!grepl(":", repository)) {
      repository <- paste0(repository, ":latest")
    }
    get_image(repository)
  }
  stevedore_object(
    "docker_image_collection",
    ## TODO: rename 'input_stream' to 'context' and then eventually to path
    ## TODO: rename 't' -> 'tag'
    ## TODO: allow specifying the stream we output to
    ## TODO: control returning output too
    ## TODO: support multiple tags (accept vector and translate into
    ##   multiple 't' parameters - not sure who needs to take
    ##   responsibility for that).
    build = docker_endpoint(
      "image_build", cl,
      hijack = quote(streaming_json(build_status_printer(stdout()))),
      after = after_build),
    get = get_image,
    list = docker_endpoint("image_list", cl),
    import = docker_endpoint("image_import", cl),
    ## TODO: need to do some argument sanitisation and renaming here.
    ## So I'm going to do this one manually
    ## TODO: rename arguments 'from_image' as 'repository'
    ## TODO: the 'after' function needs more work here - needs to know name/tag?
    pull = docker_endpoint(
      "image_create", cl,
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
    labels = function() attrs$config$labels,
    short_id = function() short_id(attrs$id),
    tags = function() setdiff(attrs$repo_tags, "<none>:<none>"),
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    history = docker_endpoint("image_history", client, fix_id_as_name),
    ## TODO: this needs to add a 'filename' option for saving
    export = docker_endpoint("image_tarball", client, fix_id_as_name),
    tag = docker_endpoint("image_tag", client, fix_id_as_name),
    ## TODO: this would best be done with a wrapper around the
    ## incoming argument for 'repo_tag' but with the core function
    ## passed through docker_endpoint so that we can pass around
    ## defaults.  We'll be doing that with other functions later too.
    untag = function(repo_tag) {
      assert_scalar_character(repo_tag)
      image_delete <- docker_endpoint("image_delete", client)
      if (!grepl(":", repo_tag, fixed = TRUE)) {
        repo_tag <- paste0(repo_tag, ":latest")
      }
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
    remove = docker_endpoint("image_delete", client, fix_id_as_name),
    reload = reload)
  self
}

docker_client_network_collection <- function(..., cl) {
  get_network <- function(id) {
    docker_client_network(id, cl)
  }
  after_create <- function(dat) {
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
  fix_id <- list(id = id)

  self <- stevedore_object(
    "docker_network",
    name = function() attrs$name,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    containers = function() lapply(attrs$containers, docker_client_container),
    connect = docker_endpoint("network_connect", client, fix_id),
    disconnect = docker_endpoint("network_disconnect", client, fix_id),
    remove = docker_endpoint("network_delete", client, fix_id),
    reload = reload)
  self
}

docker_client_volume_collection <- function(..., cl) {
  get_volume <- function(id) {
    docker_client_volume(id, cl)
  }
  after_create <- function(dat) {
    get_volume(dat$name)
  }
  after_list <- function(dat) {
    ## TODO: the NA bit can come out here later - it's there because
    ## of a type error
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
    remove = docker_endpoint("volume_delete", client, list(name = name)),
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
  after_start <- function(x) {
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
    start = docker_endpoint("exec_start", client, list(id = id),
                            hijack = quote(streaming_text(print)),
                            after = after_start),
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    resize = docker_endpoint("exec_resize", client, list(id = id)),
    reload = reload)
  self
}

## TODO: The bits below here could do with some organisation

stevedore_object <- function(class, ...) {
  els <- list(...)
  assert_named(els, TRUE, "stevedore_object elements")
  ret <- list2env(els, parent = emptyenv())
  class(ret) <- c(class, "stevedore_object")
  lock_environment(ret)
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
  if (length(x) > 0L) {
    warning(sprintf(
      "%s while %s:\n%s",
      ngettext(length(x), "warning", "warnings"),
      action,
      paste0("- %s", x, collapse = "\n")),
      call. = FALSE, immediate. = TRUE)
  }
}

subset_stevedore_object <- function(x, name) {
  .subset2(x, name) %||%
    stop(sprintf("No element '%s' within '%s' object", name, class(x)[[1]]))
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

    cat(str, file = stream)
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
      cat(x$stream, file = stream)
    }
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
