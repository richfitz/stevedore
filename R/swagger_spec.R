## Read and fetch the spec
read_spec <- function(version) {
  path <- system.file("spec", package = "stevedore", mustWork = TRUE)
  yaml <- sprintf("%s.yaml", version)
  if (!(yaml %in% dir(path))) {
    pos <- sub("\\.yaml$", "", dir(path, pattern = "^v.+\\.yaml$"))
    stop(sprintf("Invalid version %s; try one of %s",
                 version, paste(pos, collapse = ", ")))
  }
  yaml::yaml.load_file(file.path(path, yaml))
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
