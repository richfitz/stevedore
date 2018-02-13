http_client <- function(base_url = NULL, api_version = NULL, type = NULL,
                        min_version = NULL, max_version = NULL) {
  data <- http_client_data(base_url, type, is_windows())
  client_fn <- switch(data$client_type,
                      curl = http_client_curl,
                      httppipe = http_client_httppipe,
                      null = http_client_null,
                      stop("stevedore bug")) # nocov
  client_fn(data$base_url, api_version, min_version, max_version)
}


## Generate (and possibly throw) S3 errors out of http errors
##
## This probably belongs in the swagger.R file as it's being called
## from there not here.  That will leave some curl logic outside of
## this file though.
response_to_error <- function(response, endpoint, reason) {
  headers <- curl::parse_headers_list(response$headers)
  type <- headers[["content-type"]]
  if (length(response$content) > 0L) {
    if (!string_starts_with(type, "application/json")) {
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
  stop(cond)
}


## Tidy away details about url construction.  This will likely get
## more complicated as I find the corner cases.  Definitely some
## quoting/escaping etc needed here (the python client includes space
## -> '+' at least).
build_url <- function(base_url, api_version, path, params = NULL) {
  path <- paste0(path, build_url_query(params) %||% "")
  sprintf("%s/v%s%s", base_url, api_version, path)
}


build_url_query <- function(params) {
  if (length(params) > 0L) {
    stopifnot(is.list(params),
              !is.null(names(params)),
              all(nzchar(names(params))))
    to_character <- function(x) {
      if (is.logical(x)) {
        if (x) "true" else "false"
      } else {
        curl::curl_escape(as.character(x))
      }
    }

    name <- names(params)
    value <- unname(params)

    n <- lengths(value)
    if (any(n != 1L)) {
      name <- rep(name, n)
      value <- unlist(unname(params), FALSE)
    }

    q <- paste(sprintf("%s=%s", name, vcapply(value, to_character)),
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
                                    min_version = NULL, max_version = NULL) {
  min_version <- min_version %||% MIN_DOCKER_API_VERSION
  max_version <- max_version %||% MAX_DOCKER_API_VERSION
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


version_response <- function(res) {
  if (res$status_code != 200L) {
    response_to_error(res, "/_ping", "Detecting version")
  }
  raw_to_json(res$content)$ApiVersion
}


streaming_text <- function(callback) {
  assert_function(callback)
  res <- raw()
  ret <- function(x) {
    res <<- c(res, x)
    callback(decode_chunked_string(x))
  }
  attr(ret, "content") <- function() res
  ret
}


streaming_json <- function(callback) {
  assert_function(callback)
  res <- raw()
  ret <- function(x) {
    res <<- c(res, x)
    lapply(strsplit(raw_to_char(x), "\r\n")[[1]],
           function(line) callback(from_json(line)))
  }
  attr(ret, "content") <- function() res
  ret
}


http_client_data <- function(base_url = NULL, type = NULL,
                             windows = is_windows()) {
  if (is.null(base_url)) {
    base_url <- http_default_url(windows)
  } else {
    assert_scalar_character(base_url)
  }
  url_type <- http_url_type(base_url)

  if (is.null(type)) {
    type <- if (url_type == "npipe") "httppipe" else "curl"
  } else {
    type <- match_value(type, c("curl", "httppipe", "null"))
  }

  if (url_type == "npipe") {
    if (!windows) {
      stop("Named pipe connections are only available on windows")
    }
    if (type == "curl") {
      stop("The 'curl' http driver cannot connect to named pipes")
    }
  }
  if (url_type == "socket" && windows) {
    stop("Socket connections are not available on windows")
  }
  if (url_type == "http" && type == "httppipe") {
    stop("The 'httppipe' http driver cannot connect to http servers")
  }

  list(client_type = type, url_type = url_type, base_url = base_url)
}


http_url_type <- function(x) {
  assert_scalar_character(x)
  if (grepl("^npipe:/", x)) {
    type <- "npipe"
  } else if (grepl("^https?://", x)) {
    type <- "http"
  } else if (grepl("^/", x)) {
    type <- "socket"
  } else {
    stop("Can't detect url type from ", squote(x))
  }
  type
}


http_default_url <- function(windows) {
  if (windows) DEFAULT_DOCKER_WINDOWS_PIPE else DEFAULT_DOCKER_UNIX_SOCKET
}


prepare_body <- function(body) {
  if (is.raw(body)) {
    body_raw <- body
    content_type <- "application/octet-stream" # or application/x-tar
  } else {
    body_raw <- charToRaw(body)
    content_type <- "application/json"
  }
  list(raw = body_raw, content_type = content_type)
}
