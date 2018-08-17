swagger_spec_read <- function(version, quiet = FALSE, refresh = FALSE) {
  if (!refresh && version %in% names(.stevedore$spec)) {
    return(.stevedore$spec[[version]])
  }

  pos <- swagger_spec_versions()
  if (!(version %in% pos)) {
    stop(sprintf("Invalid version %s; try one of %s",
                 version, paste(pos, collapse = ", ")))
  }

  path_yml <- stevedore_file(sprintf("spec/v%s.yaml.bz2", version))
  ret <- yaml_load_file(path_yml)
  ret <- swagger_spec_patch(ret, stevedore_file("spec/patch.yaml"))
  ret <- swagger_spec_patch(ret, stevedore_file("spec/stevedore.yaml"))

  .stevedore$spec[[version]] <- ret

  ret
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

    path <- swagger_spec_path_resolve(el$path, dat)
    value <- el$value
    tmp <- dat[[path]]

    if (!is.null(el$transform)) {
      env <- parent.env(environment())
      transform <- get(el$transform, env, mode = "function", inherits = FALSE)
      tmp <- transform(tmp)
    } else if (is.null(tmp) || isTRUE(el$replace) ||
               (is.atomic(tmp) && is.atomic(value) && length(value) == 1)) {
      tmp <- value
    } else {
      tmp[names(value)] <- value
    }
    dat[[path]] <- tmp
  }

  dat
}


swagger_spec_versions <- function() {
  version_range(DOCKER_API_VERSION_MIN, DOCKER_API_VERSION_MAX)
}


swagger_spec_path_resolve <- function(path, data) {
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
      found <- vlapply(data, function(x) x[[key]] == value)
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


## Some patching drama:
patch_doc_filters <- function(x) {
  from1 <- "encoded as JSON (a `map[string][]string`)"
  to1 <- "as a named character vector"

  from2 <- paste("(A )?JSON encoded value of( the)? filters",
                 "\\(a `map\\[string\\]\\[\\]string`\\)")
  to2 <- "A named character vector of filters"

  re <- paste("(encoded as JSON \\(a `map\\[string\\]\\[\\]string`\\))\\.",
              'For example, `\\{"(.*?)": \\["(.*)"\\]\\}`')
  if (grepl(re, x)) {
    x <- sub(re, paste0(to1, '.  For example `c(\\2 = "\\3")`'), x)
  } else if (grepl(from1, x, fixed = TRUE)) {
    x <- sub(from1, to1, x, fixed = TRUE)
  } else if (grepl(from2, x)) {
    x <- sub(from2, to2, x)
  } else {
    stop("Failure to patch filter doc [stevedore bug]") # nocov
  }

  ## Fixes
  ##   - `membership=`(`accepted`|`pending`)`
  ## to
  ##   - `membership`=(`accepted`|`pending`)
  if (grepl("`)`(\n|$)", x)) {
    re <- "- `([^=`]+)=`\\(([^)]+)\\)`"
    stopifnot(grepl(re, x))
    x <- gsub(re, "- `\\1`=(\\2)", x)
  }

  x
}
