## These are the set of supported endpoints.  The format for this
## might change; I think eventually I'll end up needing to use my own
## yaml format to describe the client because we'll get min/max
## versions, and annotations for the parameter handling (which is not
## yet written)
endpoints <-
  list(ping                 = c("get",     "/_ping"),
       version              = c("get",    "/version"),
       system_events        = c("get",    "/events"),
       system_info          = c("get",    "/info"),
       system_df            = c("get",    "/system/df"),

       ## POST /containers/{id}/attach (hijack)
       ## GET /containers/{id}/logs (hijack)
       container_list       = c("get",    "/containers/json"),
       container_create     = c("post",   "/containers/create"),
       container_inspect    = c("get",    "/containers/{id}/json"),
       container_top        = c("get",    "/containers/{id}/top"),
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

       ## POST /exec/{id}/start (hijack?)
       exec_create          = c("post",   "/containers/{id}/exec"),
       ## exec_start           = c("post",   "/exec/{id}/start"),
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

docker_client_data <- function(version) {
  spec <- read_spec(version)
  list(spec = spec,
       endpoints = lapply(endpoints, function(x)
         make_endpoint(x[[1]], x[[2]], spec)))
}
