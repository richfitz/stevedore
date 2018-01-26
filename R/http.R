http_client <- function(base_url = NULL, api_version = NULL, type = NULL) {
  type <- match_value(type %||% "curl", c("curl", "httppipe"))
  switch(type,
         curl = http_client_curl(base_url, api_version),
         httppipe = http_client_httppipe(base_url, api_version),
         stop("stevedore bug")) # nocov
}

## Generate (and possibly throw) S3 errors out of http errors
##
## This probably belongs in the swagger.R file as it's being called
## from there not here.  That will leave some curl logic outside of
## this file though.
response_to_error <- function(response, pass_error, endpoint, reason) {
  headers <- curl::parse_headers_list(response$headers)
  type <- headers[["content-type"]]
  if (length(response$content) > 0L) {
    if (string_starts_with(type, "text/plain")) {
      ## This is thrown when we send junk to the server -
      ## unmarshalling errors for example.
      msg <- raw_to_char(response$content)
    } else {
      msg <- raw_to_json(response$content)$message
    }
  } else {
    ## TODO: this needs fixing but I do not know what the right answer
    ## is, frankly
    msg <- "An error occured but HEAD is obscuring it"
  }
  status_code <- response$status_code
  cond <- list(message = msg, code = status_code, endpoint = endpoint,
               reason = reason)
  class(cond) <- c("docker_error", "error", "condition")
  if (status_code %in% pass_error) {
    cond
  } else {
    stop(cond)
  }
}

## Tidy away details about url construction.  This will likely get
## more complicated as I find the corner cases.  Definitely some
## quoting/escaping etc needed here (the python client includes space
## -> '+' at least).
build_url <- function(base_url, api_version, path, params = NULL) {
  path <- paste0(path, build_url_query(params) %||% "")
  if (is.null(api_version)) {
    sprintf("%s%s", base_url, path)
  } else {
    sprintf("%s/v%s%s", base_url, api_version, path)
  }
}

## TODO: escape parameters; consider 'curl::curl_escape' but also
## gsub(' ', '+', x) which is what the python client used (though I
## don't see this mentioned in the spec itself)
build_url_query <- function(params) {
  if (length(params) > 0L) {
    stopifnot(is.list(params),
              !is.null(names(params)),
              all(nzchar(names(params))))
    to_character <- function(x) {
      if (is.logical(x)) {
        if (x) "true" else "false"
      } else {
        as.character(x)
      }
    }
    q <- paste(sprintf("%s=%s", names(params), vcapply(params, to_character)),
               collapse = "&")
    paste0("?", q)
  }
}

parse_headers <- function(headers) {
  re <- ":\\s+"
  h <- curl::parse_headers(headers)
  d <- strsplit(h[grepl(re, h)], ":\\s+")
  nms <- vcapply(d, "[[", 1L)
  vals <- vcapply(d, "[[", 2L)
  names(vals) <- nms
  vals
}

## NOTE: through here (and again through the package code that calls
## this) we need to be able to pin down the maxmimum api version - the
## swagger spec is published only up to 1.33 but the mac version
## (experimental) uses the 1.34 spec.  So we should do something like:
##
## * detect version
## * while above floor:
##   * check to see if version available locally
##   * check remotely
##   * decrement version
## * store that information somewhere sensible perhaps?
##
## This behaviour would be modified by:
##
## * do we ever look? Or require a match?
## * do we ever look *remotely*
## * what is our floor version number
http_client_api_version <- function(api_version, detect,
                                    min_version = MIN_DOCKER_API_VERSION,
                                    max_version = MAX_DOCKER_API_VERSION) {
  version_type <- "Requested"
  if (is.null(api_version)) {
    api_version <- DEFAULT_DOCKER_API_VERSION
  } else if (inherits(api_version, "numeric_version")) {
    assert_scalar(api_version)
    api_version <- as.character(api_version)
  } else {
    assert_scalar_character("api_version")
    if (tolower(api_version) == "detect") {
      api_version <- detect()
      version_type <- "Detected"
    } else {
      numeric_version(api_version) # or throw
    }
  }

  if (numeric_version(api_version) > numeric_version(max_version)) {
    message(sprintf(
      "%s API version '%s' is above max version '%s'; downgrading",
      version_type, api_version, max_version))
    api_version <- max_version
  }
  if (numeric_version(api_version) < numeric_version(min_version)) {
    message(sprintf(
      "%s API version '%s' is below min version '%s'; upgrading",
      version_type, api_version, min_version))
    api_version <- min_version
  }

  api_version
}

## This is the lowest level of the streaming functions - others can be
## done in terms of this.
streaming_raw <- function(callback = NULL) {
  res <- raw()
  if (is.null(callback)) {
    ret <- function(x) {
      res <<- c(res, x)
    }
  } else {
    ret <- function(x) {
      res <<- c(res, x)
      callback(x)
    }
  }
  attr(ret, "content") <- function() res
  ret
}

streaming_text <- function(callback = NULL) {
  force(callback)
  streaming_raw(function(x) callback(decode_chunked_string(x)))
}

streaming_json <- function(callback) {
  force(callback)
  res <- raw()
  ret <- function(x) {
    res <<- c(res, x)
    lapply(strsplit(raw_to_char(x), "\r\n")[[1]],
           function(line) callback(from_json(line)))
  }
  attr(ret, "content") <- function() res
  ret
}
