stevedore_read_index <- function() {
  path <- system.file("spec/index.yaml", package = "stevedore", mustWork = TRUE)
  dat <- yaml::yaml.load_file(path)
  names(dat) <- sub("^v", "", names(dat))
  dat
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

  patch <- yaml::yaml.load_file(system.file(
    "spec/patch.yaml", package = "stevedore"))

  ret <- spec_apply_patch(ret, patch)

  ## This bit of patching is additional to the bits in yaml
  if (version == "1.29") {
    p <- c("paths", "/containers/{id}/archive", "put", "parameters")
    tmp <- ret[[p]]
    i <- which(vcapply(tmp, "[[", "name") == "inputStream")
    tmp[[i]]$schema$format <- "binary"
    ret[[p]] <- tmp
  }

  ret
}

fetch_spec <- function(version, path) {
  url <- sprintf("https://docs.docker.com/engine/api/v%s/swagger.yaml", version)
  dest <- file.path(path, sprintf("v%s.yaml", version))
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

spec_apply_patch <- function(dat, patch) {
  v <- numeric_version(dat$info$version)

  for (el in patch) {
    if (version_check(v, el$version)) {
      path <- el$path
      value <- el$value
      tmp <- dat[[path]]
      if (is.null(tmp) || isTRUE(el$replace)) {
        tmp <- value
      } else {
        tmp[names(value)] <- value
      }
      dat[[path]] <- tmp
    }
  }

  dat
}

spec_versions <- function() {
  min_version <- unclass(numeric_version(MIN_DOCKER_API_VERSION))[[c(1, 2)]]
  max_version <- unclass(numeric_version(MAX_DOCKER_API_VERSION))[[c(1, 2)]]
  sprintf("1.%d", min_version:max_version)
}

## This is used only in the package Makefile
write_spec_index <- function(path) {
  versions <- spec_versions()
  files <- vapply(versions, fetch_spec, character(1), path)
  md5 <- tools::md5sum(files)
  names(md5) <- paste0("v", names(files))
  writeLines(yaml::as.yaml(as.list(md5)), file.path(path, "index.yaml"))
}

version_check <- function(v, cmp) {
  v <- numeric_version(v)
  cmp <- numeric_version(cmp)
  (length(cmp) == 1 && cmp == v) ||
    (length(cmp) == 2 && v >= cmp[[1]]) ||
    (length(cmp) == 2 && v <= cmp[[2]])
}
