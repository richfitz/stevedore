`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}
