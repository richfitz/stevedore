`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
`%&&%` <- function(a, b) {
  if (is.null(a)) a else b
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
    x[[el]] %||% missing
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

download_file <- function(url, dest, quiet = FALSE) {
  if (!file.exists(dest)) {
    tmp <- tempfile()
    curl::curl_download(url, tmp, quiet = quiet, mode = "wb")
    file.copy(tmp, dest)
    file.remove(tmp)
  }
  dest
}

## FIXME: ID -> i_d (id)
## FIXME: OStype -> o_stype (os_type)
## FIXME: IPv4Forwarding -> i_pv4_forwarding (ipv4_forwarding)
## FIXME: IPAM -> i_pam (ipam)
## FIXME: IPv6Gateway, IPPrefixLen, IPAddress, IPAMConfig...
##
## In general - all single character entries need warning about and
## checking.  It's possible that we'll need to special case some
## words, but it looks like here that I am going to need to lowercase
## consecutive uppercase leading letters?  Though that means we can't
## easily deal with NCPU, NGoroutines, NFd, etc.  Treating `^N` in the
## current way and everything else separately might be a reasonable
## call.
pascal_to_snake <- function(x) {
  camel_to_snake(paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x))))
}

snake_to_pascal <- function(x) {
  x <- snake_to_camel(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

camel_to_snake <- function(x) {
  if (length(x) != 1L) {
    return(vcapply(x, camel_to_snake, USE.NAMES = FALSE))
  }
  re <- "(?<=[^A-Z])([A-Z]+)"
  repeat {
    m <- regexec(re, x, perl = TRUE)[[1]]
    i <- m[[1]]
    j <- i + attr(m, "match.length")[[1]] - 1L
    if (i > 0) {
      x <- sub(re, paste0("_", tolower(substr(x, i, j))), x, perl = TRUE)
    } else {
      break
    }
  }
  x
}

snake_to_camel <- function(x) {
  if (length(x) != 1L) {
    return(vcapply(x, snake_to_camel, USE.NAMES = FALSE))
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

lock_environment <- function(env) {
  for (nm in ls(env)) {
    lockBinding(as.name(nm), env)
  }
  lockEnvironment(env)
  invisible(env)
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
}

from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

raw_to_char <- function(bin) {
  ## iconv(readBin(bin, character()), from = "UTF-8", to = "UTF-8")
  rawToChar(bin)
}

raw_to_json <- function(bin) {
  from_json(raw_to_char(bin))
}

as_call <- function(...) {
  as.call(list(...))
}

dollar <- function(...) {
  f <- function(a, b) {
    as_call(quote(`$`), a, b)
  }
  args <- list(...)
  ret <- args[[1]]
  for (i in seq_along(args)[-1L]) {
    ret <- f(ret, args[[i]])
  }
  ret
}

string_starts_with <- function(x, sub) {
  substr(x, 1, nchar(sub)) == sub
}

reset_line <- function(stream, width) {
  if (isatty(stream)) {
    cat(paste0(c("\r", strrep(" ", width), "\r"), collapse = ""), file = stream)
  }
}

capture_args <- function(f, name, indent = 4) {
  w <- getOption("width")
  oo <- options(width = w - indent)
  on.exit(options(oo))
  args <- utils::capture.output(args(f))
  txt <- sub("^function ", name, args[-length(args)])
  paste0(trimws(sprintf("%s%s\n", strrep(" " , indent), txt), "right"),
         collapse = "\n")
}

is_directory <- function(x) {
  file.exists(x) & file.info(x, extra_cols = FALSE)$isdir
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
  utils::tar(tmp, ".")
  readBin(tmp, raw(), file.size(tmp))
}
