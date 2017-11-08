stevedore_read_index <- function() {
  path <- system.file("spec/index.yaml", package = "stevedore", mustWork = TRUE)
  yaml::yaml.load_file(path)
}

read_spec <- function(version) {
  ## TODO: in theory we should be ok for versions that are _greater_
  ## than the biggest we have, but with a warning, so long as the file
  ## can be found on the docker website.  There's a bit of testing to
  ## get all of that right though.
  ##
  ## TODO: I've removed it for now, but this can be memoised easily
  ## (but reading takes only 1/20s so it's not really worth it and we
  ## might do the memoisation later than this).
  path <- spec_path()
  pos <- names(.stevedore$index)
  if (!(version %in% pos)) {
    stop(sprintf("Invalid version %s; try one of %s",
                 version, paste(pos, collapse = ", ")))
  }
  path_yml <- fetch_spec(version, path)
  md5_found <- unname(tools::md5sum(path_yml))
  md5_expected <- .stevedore$index[[version]]
  if (md5_found != md5_expected) {
    stop(sprintf("Spec for %s had different md5 than expected (%s, not %s)",
                 version, md5_found, md5_expected))
  }
  ret <- yaml::yaml.load_file(path_yml)

  ## We'll need to do a bunch of these probably; they can probably
  ## also be moved into a yaml file easily enough with version ranges.
  if (version == "v1.29") {
    ret <- spec_patch(ret, c("definitions", "Mount", "properties", "Source"),
                      type = "string")
  }

  ret
}

fetch_spec <- function(version, path) {
  url <- sprintf("https://docs.docker.com/engine/api/%s/swagger.yaml", version)
  dest <- file.path(path, paste0(version, ".yaml"))
  download_file(url, dest)
}

spec_patch <- function(spec, key, ...) {
  x <- spec[[key]]
  data <- list(...)
  x[names(data)] <- data
  spec[[key]] <- x
  spec
}

spec_path <- function() {
  path <- getOption("stevedore.spec.path", NULL)
  if (is.null(path)) {
    message("The option 'stevedore.spec.path' is not set - using temporary dir")
    path <- tempfile()
    dir.create(path, TRUE)
    path_pkg <- system.file("spec", package = "stevedore", mustWork = TRUE)
    yml <- dir(path_pkg, pattern = "v[0-9]+\\.[0-9]+.yaml$", full.names = TRUE)
    file.copy(yml, path)
    options(stevedore.spec.path = path)
  }
  path
}

## This is used only in the package Makefile
write_spec_index <- function(path) {
  min_version <- 25L
  max_version <- 33L
  versions <- sprintf("v1.%d", min_version:max_version)
  files <- vapply(versions, fetch_spec, character(1), path)
  md5 <- tools::md5sum(files)
  names(md5) <- names(files)
  writeLines(yaml::as.yaml(as.list(md5)), file.path(path, "index.yaml"))
}
