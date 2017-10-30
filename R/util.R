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

## This is *not* a general purpose partialling function (c.f.,
## purrr::partial) but one designed to provide an interface via the
## function arguments of the generated function suitable for end
## users.  So preserving argument names etc is important.  It's also
## very likely that the functions being partialled are themselves
## generated and I want to make sure that is found in the right place.
## This relies on the name of the partialled argument not being the
## same as the function being partialled.
partial1 <- function(FUN, x, env = parent.frame(),
                     name = deparse(substitute(FUN))) {
  args <- formals(FUN)

  env <- new.env(parent = env)
  env[[name]] <- FUN
  env[[names(args)[[1]]]] <- x

  body <- as.call(lapply(c(name, names(args)), as.name))
  as.function(c(args[-1], body), env)
}

camel_to_snake <- function(x) {
  if (length(x) != 1L) {
    return(vcapply(x, camel_to_snake))
  }
  re <- "(?<=[^A-Z])([A-Z])"
  repeat {
    m <- regexec(re, x, perl = TRUE)[[1]][[1]]
    if (m > 0) {
      x <- sub(re, paste0("_", tolower(substr(x, m, m))), x, perl = TRUE)
    } else {
      break
    }
  }
  x
}

snake_to_camel <- function(x) {
  if (length(x) != 1L) {
    return(vcapply(x, snake_to_camel))
  }
  re <- "_([a-z])"
  repeat {
    m <- regexec(re, x, perl = TRUE)[[1]][[1]] + 1L
    if (m > 0) {
      x <- sub(re, toupper(substr(x, m, m)), x, perl = TRUE)
    } else {
      break
    }
  }
  x
}
