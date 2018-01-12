assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                    what = "a scalar character (non-NA)") {
  assert_scalar(x, name, what)
  assert_character(x, name, what)
  assert_nonmissing(x, name, what)
  invisible(x)
}

assert_scalar_integer <- function(x, strict = FALSE,
                                  name = deparse(substitute(x)),
                                  what = "a scalar integer (non-NA)") {
  assert_scalar(x, name, what)
  assert_nonmissing(x, name, what)
  assert_integer(x, strict, name, what)
  invisible(x)
}

assert_scalar_logical <- function(x, name = deparse(substitute(x)),
                                  what = "a scalar logical (non-NA)") {
  assert_scalar(x, name, what)
  assert_nonmissing(x, name, what)
  assert_logical(x, name, what)
  invisible(x)
}

assert_scalar <- function(x, name = deparse(substitute(x)), what = "scalar") {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a %s", name, what), call. = FALSE)
  }
  invisible(x)
}
assert_nonmissing <- function(x, name = deparse(substitute(x)),
                              what = "non-NA") {
  if (any(is.na(x))) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}

assert_character <- function(x, name = deparse(substitute(x)),
                             what = "a character") {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}

assert_raw <- function(x, name = deparse(substitute(x)), what = "raw") {
  if (!is.raw(x)) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}

assert_logical <- function(x, name = deparse(substitute(x)), what = "logical") {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}

assert_is <- function(x, cl, name = deparse(substitute(x)), what = NULL) {
  if (!inherits(x, cl)) {
    if (is.null(what)) {
      what <- paste("a", paste(cl, collapse = " / "))
    }
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}

assert_integer <- function(x, strict = FALSE, name = deparse(substitute(x)),
                           what = "integer") {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
    }
  }
  invisible(x)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x)),
                         what = "named") {
  nms <- names(x)
  if (is.null(nms)) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
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

assert_scalar_character_or_null <- function(x, name = deparse(substitute(x)),
                                            what = NULL) {
  if (!is.null(x)) {
    assert_scalar_character(x, name,
                            what %||% "a scalar character (non-NA), or NULL")
  }
}

assert_null <- function(x, name = deparse(substitute(x)), what = "NULL") {
  if (!is.null(x)) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
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
