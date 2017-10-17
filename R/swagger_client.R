## These are the set of supported endpoints.  The format for this
## might change; I think eventually I'll end up needing to use my own
## yaml format to describe the client because we'll get min/max
## versions, and annotations for the parameter handling (which is not
## yet written)
endpoints <-
  list(ping            = c("get",     "/_ping"),
       version         = c("get",    "/version"),
       system_events   = c("get",    "/events"),
       system_info     = c("get",    "/info"),
       system_df       = c("get",    "/system/df"),

       image_delete    = c("delete", "/images/{name}"),
       image_tarball   = c("get",    "/images/{name}/get"),
       image_history   = c("get",    "/images/{name}/history"),
       image_inspect   = c("get",    "/images/{name}/json"),
       image_export    = c("get",    "/images/get"),
       image_list      = c("get",    "/images/json"),

       network_list    = c("get",    "/networks"),
       network_inspect = c("get",    "/networks/{id}"),
       network_prune   = c("post",   "/networks/prune"),
       network_create  = c("post",   "/networks/create"),
       network_delete  = c("delete", "/networks/{id}"))

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
