## The design through here mimics the Python docker library; we have a
## concept of a "foo collection" (e.g., a container collection) that
## produces instances of "foo" objects (e.g., containers).  This will
## be replicated for networks, volumes, etc.  Unlike the Python
## inteface we're not doing this with any fancy inheritance etc.
docker_client <- function(..., api_version = NULL) {
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
    events = strip_api_args("system_events", cl$endpoints),
    df = strip_api_args("system_df", cl$endpoints),
    info = strip_api_args("system_info", cl$endpoints),
    login = strip_api_args("system_auth", cl$endpoints),
    ping = strip_api_args("system_ping", cl$endpoints),
    version = strip_api_args("system_version", cl$endpoints))
}

docker_client_container_collection <- function(..., cl) {
  get_container <- function(id) {
    docker_client_container(id, cl)
  }
  after_create <- function(dat) {
    if (!is.na(dat$warnings)) {
      warning(dat$warnings, call. = FALSE, immediate. = TRUE)
    }
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
    ## TODO:
    ## run - this one is complex
    create = modify_args(cl$endpoints$container_create, .internal_args,
                         after = after_create, name = "container_create"),
    get = get_container,
    list = modify_args(cl$endpoints$container_list,
                       after = after_list, name = "container_list"),
    remove = strip_api_args("container_delete", cl$endpoints),
    prune = strip_api_args("container_prune", cl$endpoints))
}

docker_client_container <- function(id, client) {
  attrs <- client$endpoints$container_inspect(id)
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- client$endpoints$container_inspect(id)
    invisible(self)
  }
  make_fn <- function(name, fix_name = FALSE) {
    fix <- if (fix_name) list(name = attrs$name) else list(id = id)
    modify_args(client$endpoints[[name]], .internal_args, fix, name = name)
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
    ## attach = make_fn("container_attach"), # needs to hijack?
    commit = make_fn("image_commit", TRUE),
    diff = make_fn("container_changes"),
    ## TODO: exec (complex - start a new exec instance)
    export = make_fn("container_export"),
    path_stat = modify_args(client$endpoints$container_path_stat,
                            .internal_args, fix = list(id = id),
                            after = after_path_stat,
                            name = "container_path_stat"),
    get_archive = make_fn("container_archive"),
    put_archive = make_fn("container_import"),
    kill = make_fn("container_kill"),
    ## TODO: stdout/stderr args become TRUE by default?
    logs = make_fn("container_logs"),
    pause = make_fn("container_pause"),
    ## This should invalidate our container afterwards
    remove = make_fn("container_delete"),
    ## This might force refresh?
    rename = make_fn("container_rename"),
    resize = make_fn("container_resize"),
    restart = make_fn("container_restart"),
    start = make_fn("container_start"),
    ## TODO: expose stream (but with nice printing?)
    stats = modify_args(client$endpoints$container_stats,
                        .internal_args, fix = list(id = id, stream = FALSE),
                        name = "container_stats"),
    stop = make_fn("container_stop"),
    top = modify_args(client$endpoints$container_top,
                      .internal_args, fix = list(id = id),
                      after = after_top, name = "container_top"),
    unpause = make_fn("container_unpause"),
    update = modify_args(client$endpoints$container_update,
                         .internal_args, fix = list(id = id),
                         after = after_update, name = "container_update"),
    wait = make_fn("container_wait"),
    reload = reload)
  self
}

docker_client_image_collection <- function(..., cl) {
  get_image <- function(id) {
    docker_client_image(id, cl)
  }
  stevedore_object(
    "docker_image_collection",
    build = modify_args(cl$endpoints$image_build, .internal_args,
                        after = get_image, name = "image_build"),
    get = get_image,
    list = strip_api_args("image_list", cl$endpoints),
    import = strip_api_args("image_import", cl$endpoints),
    ## pull = strip_api_args("image_pull", cl$endpoints), # via create?
    push = strip_api_args("image_push", cl$endpoints),
    search = strip_api_args("image_search", cl$endpoints),
    remove = strip_api_args("image_delete", cl$endpoints),
    prune = strip_api_args("image_prune", cl$endpoints))
}

docker_client_image <- function(id, client) {
  attrs <- client$endpoints$image_inspect(id)
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- client$endpoints$image_inspect(id)
    invisible(self)
  }
  make_fn <- function(name, fix_name = FALSE) {
    ## NOTE: this treatment differs to that for containers
    fix <- if (fix_name) list(name = id) else list(id = id)
    modify_args(client$endpoints[[name]],
                .internal_args, fix, name = name)
  }
  self <- stevedore_object(
    "docker_image",
    id = function() attrs$id,
    labels = function() attrs$config$labels,
    short_id = function() short_id(attrs$id),
    tags = function() attrs$repo_tags[attrs$repo_tags != "<none>:<none>"],
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    history = make_fn("image_history", TRUE),
    ## TODO: this needs to add a 'filename' option for saving
    export = make_fn("image_tarball", TRUE),
    tag = make_fn("image_tag", TRUE),
    ## TODO: this would best be done with a wrapper around the
    ## incoming argument for 'repo_tag' but with the core function
    ## passed through make_fn/modify_args so that we can pass around
    ## defaults.  We'll be doing that with other functions later too.
    untag = function(repo_tag) {
      assert_scalar_character(repo_tag)
      if (!grepl(":", repo_tag, fixed = TRUE)) {
        repo_tag <- paste0(repo_tag, ":latest")
      }
      valid <- setdiff(client$endpoints$image_inspect(id)$repo_tags,
                       "<none:<none>")
      if (!(repo_tag %in% valid)) {
        stop(sprintf("Invalid repo_tag '%s' for image '%s'",
                     repo_tag, attrs$id))
      }
      client$endpoints$image_delete(repo_tag, noprune = TRUE)
    },
    ## NOTE: this removes by *id* which will not always work without a
    ## force - the name is not preserved on the way through this
    ## function.  Doing that might make more sense perhaps?
    remove = make_fn("image_delete", TRUE),
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
    create = modify_args(cl$endpoints$network_create, .internal_args,
                         after = after_create, name = "network_create"),
    get = get_network,
    list = strip_api_args("network_list", cl$endpoints),
    remove = strip_api_args("network_delete", cl$endpoints),
    prune = strip_api_args("network_prune", cl$endpoints))
}

docker_client_network <- function(id, client) {
  attrs <- client$endpoints$network_inspect(id)
  id <- attrs$id
  self <- NULL
  reload <- function() {
    attrs <<- client$endpoints$network_inspect(id)
    invisible(self)
  }
  make_fn <- function(name, fix_name = FALSE) {
    fix <- if (fix_name) list(name = attrs$name) else list(id = id)
    modify_args(client$endpoints[[name]],
                .internal_args, fix, name = name)
  }

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  ##
  ## TODO: the vast bulk of this can be done more nicely with a simple
  ## list of functions.  That will plug into an eventual help system.
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
    ## TODO: run container through container_id()
    connect = make_fn("network_connect"),
    disconnect = make_fn("network_disconnect"),
    remove = make_fn("network_delete"),
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
    create = modify_args(cl$endpoints$volume_create, .internal_args,
                         after = after_create, name = "volume_create"),
    get = get_volume,
    list = modify_args(cl$endpoints$volume_list,
                       after = after_list, name = "volume_list"),
    remove = strip_api_args("volume_delete", cl$endpoints),
    prune = strip_api_args("volume_prune", cl$endpoints))
}

docker_client_volume <- function(id, client) {
  attrs <- client$endpoints$volume_inspect(id)
  name <- attrs$name
  self <- NULL
  reload <- function() {
    attrs <<- client$endpoints$volume_inspect(name)
    invisible(self)
  }
  make_fn <- function(name) {
    fix <- list(name = attrs$name)
    modify_args(client$endpoints[[name]],
                .internal_args, fix, name = name)
  }

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  ##
  ## TODO: the vast bulk of this can be done more nicely with a simple
  ## list of functions.  That will plug into an eventual help system.

  self <- stevedore_object(
    "docker_volume",
    name = function() attrs$name,
    inspect = function(reload = TRUE) {
      if (reload) {
        reload()
      }
      attrs
    },
    remove = make_fn("volume_delete"),
    reload = reload)
  self
}

docker_client_base <- function(..., api_version = NULL) {
  base_url <- NULL
  api_version <- NULL
  self <- new.env(parent = emptyenv())
  self$cl <- R6_http_client$new(base_url, api_version)
  ## I think that we can combine these two a bit?
  dat <- suppressMessages(docker_client_data(self$cl$api_version))
  self$endpoints <- client_endpoints(self$cl, dat$endpoints)
  self
}

stevedore_object <- function(class, ...) {
  els <- list(...)
  nms <- names(els)
  if (is.null(nms) && any(!nzchar(nms)) && any(duplicated(nms))) {
    stop("Invalid names")
  }
  ret <- list2env(els, parent = emptyenv())
  class(ret) <- c(class, "stevedore_object")
  lock_environment(ret)
  ret
}

## There's a certain amount of transformation required here
strip_api_args <- function(name, list) {
  drop_args(list[[name]], .internal_args,
            name = name)
}

container_id <- function(container) {
  if (inherits(container, "docker_container")) {
    container$id()
  } else {
    assert_scalar_character(container)
    container
  }
}

short_id <- function(x) {
  end <- if (string_starts_with(x, "sha256:")) 17L else 10L
  substr(x, 1, end)
}

drop_leading_slash <- function(x) {
  sub("^/", "", x)
}

.internal_args <- c("pass_error", "hijack", "as_is_names")

## TODO: the NA bit can come out here later - it's there because
## of a type error
report_warnings <- function(x, action) {
  if (length(x) > 0L && !identical(x, NA_character_)) {
    warning(sprintf(
      "%s while %s:\n%s",
      ngettext(length(x), "warning", "warnings"),
      action,
      paste0("- %s", x, collapse = "\n")),
      call. = FALSE, immediate. = TRUE)
  }
}
