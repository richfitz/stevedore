## These are the set of supported endpoints.  The format for this
## might change; I think eventually I'll end up needing to use my own
## yaml format to describe the client because we'll get min/max
## versions, and annotations for the parameter handling (which is not
## yet written)
endpoints <-
  list(system_auth          = c("post",   "/auth"),
       system_info          = c("get",    "/info"),
       system_version       = c("get",    "/version"),
       system_ping          = c("get",    "/_ping"),
       system_events        = c("get",    "/events"),
       system_df            = c("get",    "/system/df"),

       container_list       = c("get",    "/containers/json"),
       container_create     = c("post",   "/containers/create"),
       container_inspect    = c("get",    "/containers/{id}/json"),
       container_top        = c("get",    "/containers/{id}/top"),
       container_logs       = c("get",    "/containers/{id}/logs"),
       container_export     = c("get",    "/containers/{id}/export"),
       container_changes    = c("get",    "/containers/{id}/changes"),
       container_stats      = c("get",    "/containers/{id}/stats"),
       container_resize     = c("post",   "/containers/{id}/resize"),
       container_start      = c("post",   "/containers/{id}/start"),
       container_stop       = c("post",   "/containers/{id}/stop"),
       container_restart    = c("post",   "/containers/{id}/restart"),
       container_kill       = c("post",   "/containers/{id}/kill"),
       container_update     = c("post",   "/containers/{id}/update"),
       container_rename     = c("post",   "/containers/{id}/rename"),
       container_pause      = c("post",   "/containers/{id}/pause"),
       container_unpause    = c("post",   "/containers/{id}/unpause"),
       container_attach     = c("post",   "/containers/{id}/attach"),
       container_wait       = c("post",   "/containers/{id}/wait"),
       container_delete     = c("delete", "/containers/{id}"),
       container_files      = c("head",   "/containers/{id}/archive"),
       container_archive    = c("get",    "/containers/{id}/archive"),
       container_import     = c("put",    "/containers/{id}/archive"),
       container_prune      = c("post",   "/containers/prune"),

       image_list           = c("get",    "/images/json"),
       image_build          = c("post",   "/build"),
       image_create         = c("post",   "/images/create"),
       image_inspect        = c("get",    "/images/{name}/json"),
       image_history        = c("get",    "/images/{name}/history"),
       image_push           = c("post",   "/images/{name}/push"),
       image_tag            = c("post",   "/images/{name}/tag"),
       image_delete         = c("delete", "/images/{name}"),
       image_search         = c("get",    "/images/search"),
       image_prune          = c("post",   "/images/prune"),
       image_commit         = c("post",   "/commit"),
       image_tarball        = c("get",    "/images/{name}/get"),
       image_export         = c("get",    "/images/get"),
       image_import         = c("post",   "/images/load"),

       network_list         = c("get",    "/networks"),
       network_inspect      = c("get",    "/networks/{id}"),
       network_delete       = c("delete", "/networks/{id}"),
       network_create       = c("post",   "/networks/create"),
       network_connect      = c("post",   "/networks/{id}/connect"),
       network_disconnect   = c("post",   "/networks/{id}/disconnect"),
       network_prune        = c("post",   "/networks/prune"),

       volume_list          = c("get",    "/volumes"),
       volume_create        = c("post",   "/volumes/create"),
       volume_inspect       = c("get",    "/volumes/{name}"),
       volume_delete        = c("delete", "/volumes/{name}"),
       volume_prune         = c("post",   "/volumes/prune"),

       exec_create          = c("post",   "/containers/{id}/exec"),
       exec_start           = c("post",   "/exec/{id}/start"),
       exec_resize          = c("post",   "/exec/{id}/resize"),
       exec_inspect         = c("get",    "/exec/{id}/json")
       )

## memoise reading the spec:
spec <- new.env(parent = emptyenv())
read_spec <- function(version) {
  if (is.null(spec[[version]])) {
    path <- system.file("spec", package = "stevedore", mustWork = TRUE)
    yaml <- sprintf("%s.yaml", version)
    if (!(yaml %in% dir(path))) {
      pos <- sub("\\.yaml$", "", dir(path, pattern = "^v.+\\.yaml$"))
      stop(sprintf("Invalid version %s; try one of %s",
                   version, paste(pos, collapse = ", ")))
    }
    spec[[version]] <- yaml::yaml.load_file(file.path(path, yaml))
  }
  spec[[version]]
}

fetch_spec <- function(version, path) {
  url <- sprintf("https://docs.docker.com/engine/api/%s/swagger.yaml", version)
  dest <- file.path(path, paste0(version, ".yaml"))
  download_file(url, dest)
}

spec_index <- function(path) {
  min_version <- 25L
  max_version <- 32L
  versions <- sprintf("v1.%d", min_version:max_version)
  files <- vapply(versions, fetch_spec, character(1), path)
  md5 <- tools::md5sum(files)
  names(md5) <- names(files)
  writeLines(jsonlite::toJSON(as.list(md5), auto_unbox = TRUE, pretty = TRUE),
             file.path(path, "index.json"))
}

docker_client_data <- function(version) {
  spec <- read_spec(version)
  list(spec = spec,
       endpoints = lapply(endpoints, function(x)
         make_endpoint(x[[1]], x[[2]], spec)))
}

client_endpoint <- function(name, env) {
  args <- formals(env$endpoints[[name]]$argument_handler)
  subs <- list(
    name = name,
    get_params = as.call(c(list(quote(endpoint$argument_handler)),
                           lapply(names(args), as.name))))
  body <- substitute(expression({
    endpoint <- endpoints$name
    params <- get_params
    run_endpoint(client, endpoint, params, pass_error, hijack)
  }), subs)[[2]]
  as.function(c(args, alist(pass_error = FALSE, hijack = FALSE), body),
              env)
}

client_endpoints <- function(client, endpoints) {
  env <- new.env(parent = baseenv())
  env$client <- client
  env$endpoints <- endpoints
  ## We can either move this over or set things so that the parent
  ## environment is namespace:stevedore
  env$run_endpoint <- run_endpoint
  lock_environment(env)
  nms <- names(endpoints)
  set_names(lapply(names(endpoints), client_endpoint, env), nms)
}
