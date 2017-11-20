## The design through here mimics the Python docker library; we have a
## concept of a "foo collection" (e.g., a container collection) that
## produces instances of "foo" objects (e.g., containers).  This will
## be replicated for networks, volumes, etc.  Unlike the Python
## inteface we're not doing this with any fancy inheritance etc.
docker_client <- function(..., api_version = NULL) {
  cl <- docker_client_base(..., api_version = api_version)

  containers <- docker_client_container_collection(cl = cl)

  stevedore_object(
    "docker_client",
    containers = containers,
    events = strip_api_args("system_events", cl$endpoints),
    df = strip_api_args("system_df", cl$endpoints),
    info = strip_api_args("system_info", cl$endpoints),
    login = strip_api_args("system_auth", cl$endpoints),
    ping = strip_api_args("system_ping", cl$endpoints),
    version = strip_api_args("system_version", cl$endpoints))
}

docker_client_container_collection <- function(..., cl) {
  stevedore_object(
    "docker_container_collection",
    ## TODO:
    ##
    ## run
    create = strip_api_args("container_create", cl$endpoints),
    get = function(id) {
      docker_client_container(id, cl)
    },
    list = strip_api_args("container_list", cl$endpoints),
    prune = strip_api_args("container_prune", cl$endpoints))
}

docker_client_container <- function(id, client) {
  attrs <- cl$endpoints$container_inspect(id)
  id <- attrs$id
  reload <- function() {
    attrs <<- cl$endpoints$container_inspect(id)
  }
  make_fn <- function(name, fix_name = FALSE) {
    fix <- if (fix_name) list(name = attrs$name) else list(id = id)
    modify_args(client$endpoints[[name]],
                c("pass_error", "hijack","as_is_names"), fix, name = name)
  }

  ## TODO: friendly "copy" interface needed here, but that requires a
  ## bit more general work really.
  ##
  ## TODO: the vast bulk of this can be done more nicely with a simple
  ## list of functions.  That will plug into an eventual help system.
  stevedore_object(
    "docker_container",
    name = function() sub("^/", "", attrs$name),
    image = function() strsplit(attrs$image, ":", fixed = TRUE)[[1L]][[2L]],
    labels = function() attrs$config$labels,
    status = function() attrs$state$status,
    ## TODO: this one is hard because it might need to hijack the connection
    ## attach = make_fn("container_attach"), # needs to hijack?
    commit = make_fn("image_commit", TRUE),
    diff = make_fn("container_changes"),
    ## TODO: exec (complex - start a new exec instance)
    export = make_fn("container_export"),
    get_archive = make_fn("container_archive"),
    put_archive = make_fn("container_import"),
    kill = make_fn("container_kill"),
    logs = make_fn("container_logs"),
    pause = make_fn("container_pause"),
    remove = make_fn("container_delete"),
    rename = make_fn("container_rename"),
    resize = make_fn("container_resize"),
    restart = make_fn("container_restart"),
    start = make_fn("container_start"),
    stats = make_fn("container_stats"),
    stop = make_fn("container_stop"),
    top = make_fn("container_top"),
    unpause = make_fn("container_unpause"),
    update = make_fn("container_update"),
    wait = make_fn("container_wait"),
    reload = reload)
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
  drop_args(list[[name]], c("pass_error", "hijack","as_is_names"),
            name = name)
}
