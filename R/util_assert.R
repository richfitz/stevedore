assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
  assert_nonmissing(x, name)
  invisible(x)
}

assert_scalar_integer <- function(x, strict = FALSE,
                                  name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_nonmissing(x, name)
  assert_integer(x, strict, name)
  invisible(x)
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_nonmissing(x, name)
  assert_logical(x, name)
  invisible(x)
}

assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
  invisible(x)
}
assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("'%s' must not be NA", name), call. = FALSE)
  }
  invisible(x)
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be a character", name), call. = FALSE)
  }
  invisible(x)
}

assert_raw <- function(x, name = deparse(substitute(x))) {
  if (!is.raw(x)) {
    stop(sprintf("'%s' must be raw", name), call. = FALSE)
  }
  invisible(x)
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
  invisible(x)
}

assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
  invisible(x)
}

assert_integer <- function(x, strict = FALSE, name = deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("'%s' must be integer", name), call. = FALSE)
    }
  }
  invisible(x)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  nms <- names(x)
  if (is.null(nms)) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (!all(nzchar(nms))) {
    stop(sprintf("All elements of '%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(nms))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}

assert_directory <- function(x, name = deparse(substitute(x))) {
  if (!is_directory(x)) {
    stop(sprintf("'%s' must be an existing directory", name), call. = FALSE)
  }
}

match_value <- function(x, values, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  if (is.na(match(x, values))) {
    stop(sprintf("'%s' must be one of %s",
                 name, paste(sprintf("'%s'", values), collapse = ", ")),
         call. = FALSE)
  }
  x
}

## It's possible that we can do this with streaming but I don't know
## that's sensible.  One option would be to run as far through as the
## tar to file step and then pass through something that we can later
## pass through to curl for streaming upload.  That would be quite a
## bit nicer but will require some cleanup later.  We can do that with
## an option through here coupled with some cleanup work in the
## process functions and significant work to run_endpoint.
tar_directory <- function(path, setwd = TRUE) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  tmp <- tempfile()
  on.exit(file.remove(tmp), add = TRUE)
  tar(tmp, ".")
  readBin(tmp, raw(), file.size(tmp))
}
