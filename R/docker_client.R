##' Create a docker client object
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help()}
##'
##' @title Create docker client
##'
##' @param api_version Version of the API request from the api.
##'   Options are \code{NULL} (the default) - use the package's
##'   default version (currently 1.29), a version as a string or
##'   \code{\link{numeric_version}} object (supported between 1.25 and
##'   1.33), or the string \code{detect} which will use the highest
##'   version out of the version reported by the api and 1.33
##'
##' @param url The URL for the docker daemon.  This can be an absolute
##'   file path (for a unix socket on macOS/Linux), a named pipe
##'   (e.g., \code{npipe:////./pipe/docker_engine}) on Windows, or
##'   (eventually) an http or https url (e.g.,
##'   \code{https://localhost:8888}), though this is not yet
##'   supported.
##'
##' @param ... Reserved for future use.  Passing in any unrecognised
##'   argument will throw an error.
##'
##' @param http_client_type HTTP client type to use.  The options are
##'   (currently) "curl", which uses the \code{curl} package (works
##'   over unix sockets and eventually over TCP) and \code{httppipe}
##'   which works over unix sockets and eventually windows named
##'   pipes, using the Docker SDK's pipe code via the \code{httppipe}
##'   package.  Not all functionality is supported with the
##'   \code{httppipe} client.  This option may eventually be moved
##'   into the \code{...} argument as is not intended for end-user
##'   use; it is primarily intended for debugging in development
##'   (forcing the \code{httppipe} client where the \code{curl} client
##'   would ordinarily be preferred).
##'
##' @export
docker_client <- function(api_version = NULL, url = NULL, ...,
                          http_client_type = NULL) {
  assert_empty_dots(..., name = "docker_client")

  ## The design through here mimics the Python docker library; we have
  ## a concept of a "foo collection" (e.g., a container collection)
  ## that produces instances of "foo" objects (e.g., containers).
  ## This will be replicated for networks, volumes, etc.  Unlike the
  ## Python inteface we're not doing this with any fancy inheritance
  ## etc.
  api_client <- docker_api_client(base_url = url, api_version = api_version,
                                  type = http_client_type, ...)

  ret <- stevedore_object(
    api_client,
    "docker_client",
    events = docker_client_method("system_events", api_client),
    df = docker_client_method("system_df", api_client),
    info = docker_client_method("system_info", api_client),
    login = docker_client_method("system_auth", api_client),
    ping = docker_client_method("system_ping", api_client),
    version = docker_client_method("system_version", api_client),
    api_version = function() api_client$http_client$api_version,
    lock = FALSE)

  ret$containers <- docker_client_container_collection(api_client, ret)
  ret$images <- docker_client_image_collection(api_client, ret)
  ret$networks <- docker_client_network_collection(api_client, ret)
  ret$volumes <- docker_client_volume_collection(api_client, ret)

  lock_environment(ret)
  ret
}

docker_client_container_collection <- function(api_client, parent) {
  get_container <- function(id) {
    docker_client_container(id, api_client)
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
    api_client,
    "docker_container_collection",
    run = make_docker_run(parent, api_client$http_client$can_stream),
    create = docker_client_method(
      "container_create", api_client,
      promote = c("image", "cmd"),
      rename = c(ports = "exposed_ports", network = "networking_config"),
      defaults = alist(image =),
      process = list(
        quote(image <- get_image_id(image)),
        quote(cmd <- check_command(cmd)),
        mcr_volumes_for_create(quote(volumes), quote(host_config)),
        mcr_ports_for_create(quote(ports), quote(host_config)),
        mcr_network_for_create(quote(network), quote(host_config))),
      after = after_create),
    get = get_container,
    list = docker_client_method(
      "container_list", api_client,
      process = list(quote(filters <- as_docker_filter(filters))),
      after = after_list),
    remove = docker_client_method(
      "container_delete", api_client,
      rename = c(delete_volumes = "v"),
      process = list(quote(id <- get_image_id(id)))),
    prune = docker_client_method("container_prune", api_client))
}

docker_client_container <- function(id, api_client) {
  container_inspect <- docker_client_method("container_inspect", api_client)
  attrs <- if (identical(id, HELP)) NULL else container_inspect(id)
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
    ret <- docker_client_exec(x$id, api_client)
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
  after_start <- function(x, ...) {
    invisible(self)
  }
  after_top <- function(x, ...) {
    m <- matrix(unlist(x$processes), byrow = TRUE, nrow = length(x$processes))
    colnames(m) <- x$titles
    ## NOTE: some of these can be non-text.  Not sure how to safely do
    ## that though.  So for now it's all going to be character.
    as.data.frame(m, stringsAsFactors = FALSE)
  }
  after_update <- function(x, ...) {
    report_warnings(x$warnings, "updating container")
    invisible(self)
  }
  after_commit <- function(x, ...) {
    docker_client_image(x$id, api_client)
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
  status <- function(reload = TRUE) {
    self$inspect(reload)$state$status
  }
  fix_id <- list(id = id)

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  self <- stevedore_object(
    api_client,
    "docker_container",
    id = function() id,
    name = function() drop_leading_slash(attrs$name),
    image = function() {
      docker_client_image(
        strsplit(attrs$image, ":", fixed = TRUE)[[1L]][[2L]], api_client)
    },
    labels = function() attrs$config$labels,
    status = status,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    ## TODO: "attach" is hard because it might need to hijack the
    ## connection and deal with stdin (follow logs is close but not
    ## quite the same)
    ## attach = docker_client_method("container_attach", api_client,
    ##    fix = fix_id)
    ##
    ## NOTE: The promotion list for commit is to mimic the argument
    ## list for the command line version of `docker commit` (minus
    ## "id" which is fixed).
    commit = docker_client_method(
      "image_commit", api_client,
      promote = c("repo", "tag", "author", "changes", "comment", "pause"),
      fix = list(container = id), after = after_commit),
    diff = docker_client_method(
      "container_changes", api_client, fix = fix_id),
    ## TODO: inject 'start' into here too, which then requires passing
    ## detach through as well and dealing with those through the
    ## 'after' function.
    exec = docker_client_method(
      "exec_create", api_client, fix = fix_id,
      rename = c(stdout = "attach_stdout", stderr = "attach_stderr",
                 stdin = "attach_stdin"),
      defaults = alist(stdout = TRUE, stderr = TRUE, cmd =),
      promote = "cmd",
      process = list(quote(cmd <- check_command(cmd))),
      after = after_exec),
    export = docker_client_method(
      "container_export", api_client, fix = fix_id),
    path_stat = docker_client_method(
      "container_path_stat", api_client, fix = fix_id,
      after = after_path_stat),
    ##
    get_archive = docker_client_method(
      "container_archive", api_client, fix = fix_id, extra = alist(dest =),
      process = list(quote(assert_scalar_character_or_null(dest))),
      after = after_get_archive),
    ## TODO: option for compression, pass through to tar file (much
    ## easier to get right if we can rely on R tar)
    put_archive = docker_client_method(
      "container_import", api_client, fix = fix_id,
      rename = c(src = "input_stream"),
      process = list(quote(src <- validate_tar_input(src))),
      after = nothing),
    ##
    kill = docker_client_method(
      "container_kill", api_client, fix = fix_id),
    ## Logs; quite complicated in the case of 'follow'
    ## -  stream has an effect *only* if follow is TRUE
    logs = docker_client_method(
      "container_logs", api_client, fix = fix_id,
      defaults = list(stdout = TRUE, stderr = TRUE),
      process = list(
        quote(if (is.numeric(tail)) tail <- as.character(tail)),
        mcr_prepare_stream_and_close(quote(stream))),
      extra = alist(stream = stdout()),
      hijack = quote(if (isTRUE(follow))
                       streaming_text(docker_stream_printer(stream))),
      allow_hijack_without_stream = FALSE,
      after = after_logs),
    pause = docker_client_method(
      "container_pause", api_client, fix = fix_id),
    ## This should invalidate our container afterwards
    ## NOTE: consider using parent?
    remove = docker_client_method(
      "container_delete", api_client, fix = fix_id,
      rename = c(delete_volumes = "v")),
    ## This might force refresh?
    rename = docker_client_method(
      "container_rename", api_client, fix = fix_id),
    resize = docker_client_method(
      "container_resize", api_client, fix = fix_id),
    restart = docker_client_method(
      "container_restart", api_client, fix = fix_id),
    start = docker_client_method(
      "container_start", api_client, fix = fix_id, after = after_start),
    ## TODO: expose stream (but with nice printing and escape instructions?)
    stats = docker_client_method(
      "container_stats", api_client, fix = c(fix_id, stream = FALSE)),
    stop = docker_client_method(
      "container_stop", api_client, fix = fix_id),
    top = docker_client_method(
      "container_top", api_client, fix = fix_id, after = after_top),
    unpause = docker_client_method(
      "container_unpause", api_client, fix = fix_id),
    update = docker_client_method(
      "container_update", api_client, fix = fix_id, after = after_update),
    wait = docker_client_method(
      "container_wait", api_client, fix = fix_id),
    ports = ports,
    reload = reload)
  self
}

docker_client_image_collection <- function(api_client, parent) {
  get_image <- function(id) {
    docker_client_image(id, api_client)
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
    get_image(sprintf("%s:%s", params$query$fromImage, params$query$tag))
  }
  stevedore_object(
    api_client,
    "docker_image_collection",
    ## TODO: control returning output too
    ## TODO: support multiple tags (accept vector and translate into
    ##   multiple 't' parameters - needs support in generated handlers
    build = docker_client_method(
      "image_build", api_client,
      drop = "content_type",
      rename = c(context = "input_stream", tag = "t"),
      defaults = alist(context =),
      extra = alist(verbose = NULL, stream = stdout()),
      process = list(
        mcr_prepare_stream_and_close(quote(stream)),
        quote(context <- validate_tar_directory(context))),
      hijack = quote(streaming_json(build_status_printer(stream))),
      allow_hijack_without_stream = TRUE,
      after = after_build),
    get = get_image,
    list = docker_client_method("image_list", api_client),
    import = docker_client_method("image_import", api_client),
    ## TODO: add filename argument for saving (see image_tarball)
    export = docker_client_method(
      "image_export", api_client,
      process = list(quote(validate_export_names(names)))),
    ## TODO: need to deal with registry auth properly.  For now I'm just
    ##   eliminating it from the list.
    pull = docker_client_method(
      "image_create", api_client, rename = c("name" = "from_image"),
      drop = c("input_image", "from_src", "repo", "registry_auth"),
      process = list(
        mcr_process_image_and_tag(quote(name), quote(tag)),
        mcr_prepare_stream_and_close(quote(stream))),
      extra = alist(stream = stdout()),
      defaults = alist(name =),
      hijack = quote(streaming_json(pull_status_printer(stream))),
      allow_hijack_without_stream = TRUE,
      after = after_pull),
    push = docker_client_method("image_push", api_client),
    search = docker_client_method("image_search", api_client),
    remove = docker_client_method("image_delete", api_client),
    prune = docker_client_method("image_prune", api_client))
}

docker_client_image <- function(id, api_client) {
  image_inspect <- docker_client_method("image_inspect", api_client)
  attrs <- if (identical(id, HELP)) NULL else image_inspect(id)
  name <- id
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- image_inspect(id)
    invisible(self)
  }
  invisible_self <- function(...) {
    invisible(self$reload())
  }
  ## TODO: repo and tag should be separate as for tag (with option
  ## to do them together).
  untag <- function(repo_tag) {
    repo_tag <- image_name(repo_tag)
    valid <- setdiff(self$inspect()$repo_tags, "<none>:<none>")
    if (!(repo_tag %in% valid)) {
      stop(sprintf("Invalid repo_tag '%s' for image '%s'",
                   repo_tag, attrs$id))
    }
    ## NOTE: this is a little awkward - we can't use self$remove()
    ## because that refers to the actual iage id, which is not what we
    ## want.  So we rebuild the endpoint without the `fix` argument
    ## and then call it with just the tag.
    docker_client_method("image_delete", api_client)(repo_tag, noprune = TRUE)
    invisible(self$reload())
  }
  fix_id_as_name = list(name = id)
  self <- stevedore_object(
    api_client,
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
    history = docker_client_method(
      "image_history", api_client, fix = fix_id_as_name),
    ## TODO: this needs to add a 'filename' option for saving
    export = docker_client_method(
      "image_tarball", api_client, fix = fix_id_as_name),
    tag = docker_client_method(
      "image_tag", api_client, fix = fix_id_as_name,
      after = invisible_self, defaults = alist(repo =)),
    untag = untag,
    ## NOTE: this always tries to remove the image by *id* not by
    ## name, which is not ideal really.  When force = TRUE it's
    ## basically the same I think.
    remove = docker_client_method(
      "image_delete", api_client, fix = fix_id_as_name),
    reload = reload)
  self
}

docker_client_network_collection <- function(api_client, parent) {
  get_network <- function(id) {
    docker_client_network(id, api_client)
  }
  after_create <- function(dat, ...) {
    get_network(dat$id)
  }
  stevedore_object(
    api_client,
    "docker_network_collection",
    create = docker_client_method(
      "network_create", api_client, after = after_create,
      defaults = alist(check_duplicate = TRUE)),
    get = get_network,
    list = docker_client_method("network_list", api_client),
    remove = docker_client_method("network_delete", api_client),
    prune = docker_client_method("network_prune", api_client))
}

docker_client_network <- function(id, api_client) {
  network_inspect <- docker_client_method("network_inspect", api_client)
  attrs <- if (identical(id, HELP)) NULL else network_inspect(id)
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- network_inspect(id)
    invisible(self)
  }
  containers <- function(reload = TRUE) {
    containers <- self$inspect(reload)$containers
    lapply(names(containers), docker_client_container, api_client)
  }
  fix_id <- list(id = id)

  self <- stevedore_object(
    api_client,
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
    connect = docker_client_method(
      "network_connect", api_client, fix = fix_id),
    disconnect = docker_client_method(
      "network_disconnect", api_client, fix = fix_id),
    remove = docker_client_method(
      "network_delete", api_client, fix = fix_id),
    reload = reload)
  self
}

docker_client_volume_collection <- function(api_client, parent) {
  get_volume <- function(id) {
    docker_client_volume(id, api_client)
  }
  after_create <- function(dat, ...) {
    get_volume(dat$name)
  }
  after_list <- function(dat, ...) {
    report_warnings(dat$warnings, "reading volume list")
    dat$volumes
  }
  stevedore_object(
    api_client,
    "docker_volume_collection",
    create = docker_client_method(
      "volume_create", api_client, after = after_create),
    get = get_volume,
    list = docker_client_method("volume_list", api_client, after = after_list),
    remove = docker_client_method("volume_delete", api_client),
    prune = docker_client_method("volume_prune", api_client))
}

docker_client_volume <- function(id, api_client) {
  volume_inspect <- docker_client_method("volume_inspect", api_client)
  attrs <- if (identical(id, HELP)) NULL else volume_inspect(id)
  name <- attrs$name
  self <- NULL
  reload <- function() {
    attrs <<- volume_inspect(name)
    invisible(self)
  }

  self <- stevedore_object(
    api_client,
    "docker_volume",
    name = function() attrs$name,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    map = function(path, readonly = FALSE) {
      assert_scalar_character(path)
      assert_scalar_logical(readonly)
      fmt <- "%s:%s"
      if (readonly) {
        fmt <- paste0(fmt, ":ro")
      }
      sprintf(fmt, name, path)
    },
    remove = docker_client_method(
      "volume_delete", api_client, fix = list(name = name)),
    reload = reload)
  self
}

docker_client_exec <- function(id, api_client) {
  exec_inspect <- docker_client_method("exec_inspect", api_client)
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
    api_client,
    "docker_exec",
    id = function() id,
    ## TODO: control stream (location etc) following the same problem
    ## in build.
    ## TODO: explicitly set 'detach' argument
    start = docker_client_method(
      "exec_start", api_client, fix = list(id = id),
      extra = alist(stream = stdout()),
      hijack = quote(streaming_text(docker_stream_printer(stream))),
      allow_hijack_without_stream = TRUE,
      process = list(mcr_prepare_stream_and_close(quote(stream))),
      after = after_start),
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    resize = docker_client_method(
      "exec_resize", api_client, fix = list(id = id)),
    reload = reload)
  self
}
