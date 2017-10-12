`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

require_version_ge <- function(required, server) {
  if (is.null(server) || numeric_version(server) >= numeric_version(needed)) {
    invisible(TRUE)
  } else {
    stop(api_version_error(required, server))
  }
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

substr_len <- function(x, start, len) {
  substr(x, start, start + len - 1L)
}
