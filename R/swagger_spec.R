swagger_spec_index <- function(refresh = FALSE) {
  if (is.null(.stevedore$index) || refresh) {
    path <- stevedore_file("spec/index.yaml")
    dat <- yaml_load_file(path)
    names(dat) <- sub("^v", "", names(dat))
    .stevedore$index <- dat
  }
  .stevedore$index
}


swagger_spec_read <- function(version, refresh = FALSE) {
  if (!refresh && version %in% names(.stevedore$spec)) {
    return(.stevedore$spec[[version]])
  }
  ## NOTE: in theory we should be ok for versions that are _greater_
  ## than the biggest we have, but with a warning, so long as the file
  ## can be found on the docker website.  There's a bit of testing to
  ## get all of that right though.  That should be dealt with at a
  ## higher level than this function though, which should just read a
  ## spec.
  path <- swagger_spec_path()
  spec_index <- swagger_spec_index()

  pos <- names(spec_index)
  if (!(version %in% pos)) {
    stop(sprintf("Invalid version %s; try one of %s",
                 version, paste(pos, collapse = ", ")))
  }
  path_yml <- swagger_spec_fetch(version, path)
  md5_found <- hash_file(path_yml)
  md5_expected <- spec_index[[version]]
  if (md5_found != md5_expected) {
    stop(sprintf("Spec for %s had different md5 than expected (%s, not %s)",
                 version, md5_found, md5_expected))
  }
  ret <- yaml_load_file(path_yml)
  ret <- swagger_spec_patch(ret, stevedore_file("spec/patch.yaml"))
  ret <- swagger_spec_patch(ret, stevedore_file("spec/stevedore.yaml"))

  .stevedore$spec[[version]] <- ret

  ret
}


swagger_spec_fetch <- function(version, path) {
  url <- sprintf("https://docs.docker.com/engine/api/v%s/swagger.yaml", version)
  dest <- file.path(path, sprintf("v%s.yaml", version))
  quiet <- getOption("stevedore.silent", FALSE)
  download_file(url, dest, quiet)
}


swagger_spec_path <- function() {
  path <- getOption("stevedore.spec.path", NULL)
  if (is.null(path)) {
    if (!getOption("stevedore.silent", FALSE)) {
      message("The option 'stevedore.spec.path' not set - using temporary dir")
    }
    path <- tempfile()
    dir.create(path, TRUE)
    path_pkg <- stevedore_file("spec")
    yml <- dir(path_pkg, pattern = "v[0-9]+\\.[0-9]+.yaml$", full.names = TRUE)
    file.copy(yml, path)
    options(stevedore.spec.path = path)
  }
  path
}


swagger_spec_patch <- function(dat, patch_file) {
  patch <- yaml_load_file(patch_file)
  v <- numeric_version(dat$info$version)
  for (el in patch) {
    if (!is.null(el$version)) {
      assert_character(el$version)
      if (!version_check(v, el$version)) {
        next
      }
    }

    path <- swagger_path_resolve(el$path, dat)
    value <- el$value
    tmp <- dat[[path]]
    if (is.null(tmp) || isTRUE(el$replace)) {
      tmp <- value
    } else {
      tmp[names(value)] <- value
    }
    dat[[path]] <- tmp
  }

  dat
}


swagger_spec_versions <- function() {
  min_version <- unclass(numeric_version(MIN_DOCKER_API_VERSION))[[c(1, 2)]]
  max_version <- unclass(numeric_version(MAX_DOCKER_API_VERSION))[[c(1, 2)]]
  sprintf("1.%d", min_version:max_version)
}


## This is used only in the package Makefile
swagger_spec_index_write <- function(path) {
  versions <- swagger_spec_versions()
  files <- vapply(versions, swagger_spec_fetch, character(1), path)
  md5 <- hash_file(files)
  names(md5) <- paste0("v", names(files))
  cat(yaml::as.yaml(as.list(md5)), file = file.path(path, "index.yaml"))
}


swagger_path_resolve <- function(path, data) {
  needs_resolve <- string_starts_with(path, "@")
  if (!any(needs_resolve)) {
    return(path)
  }

  re <- "@(.+?)\\s*=\\s*(.+)$"
  stopifnot(grepl(re, path[needs_resolve]))

  ret <- integer(length(path))
  for (i in seq_along(path)) {
    if (needs_resolve[[i]]) {
      key <- sub(re, "\\1", path[[i]])
      value <- sub(re, "\\2", path[[i]])
      found <- vapply(data, function(x) x[[key]] == value, logical(1))
      stopifnot(sum(found) == 1L)
      j <- which(found)
    } else  {
      j <- match(path[[i]], names(data))
      stopifnot(!is.na(j))
    }
    ret[[i]] <- j
    data <- data[[j]]
  }

  ret
}
