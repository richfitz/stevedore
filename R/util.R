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

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

substr_len <- function(x, start, len) {
  substr(x, start, start + len - 1L)
}

as_na <- function(x) {
  x[] <- NA
  x
}

pick <- function(x, el, missing) {
  if (el %in% names(x)) {
    x[[el]]
  } else {
    missing
  }
}

set_attributes <- function(x, attr) {
  for (i in names(attr)) {
    attr(x, i) <- attr[[i]]
  }
  x
}

read_pending <- function(con, what = raw()) {
  dat <- what
  while (isIncomplete(con)) {
    res <- readBin(con, what, 1024)
    if (length(res) == 0L) {
      break
    }
    dat <- c(dat, res)
  }
  dat
}

download_file <- function(url, dest, quiet = FALSE) {
  if (!file.exists(dest)) {
    tmp <- tempfile()
    curl::curl_download(url, tmp, quiet = quiet, mode = "wb")
    file.copy(tmp, dest)
    file.remove(tmp)
  }
  dest
}
