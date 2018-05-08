http_client <- function(config, min_version = NULL, max_version = NULL) {
  client_fn <- switch(config$http_client_type,
                      curl = http_client_curl,
                      httppipe = http_client_httppipe,
                      null = http_client_null,
                      stop("stevedore bug")) # nocov
  ret <- client_fn(config, min_version, max_version)
  ret$config <- config
  ret
}


connection_info <- function(client) {
  config <- client$config
  list(api_version = client$api_version,
       protocol = config$protocol,
       host = config$addr,
       http_client_type = config$http_client_type,
       use_tls = config$use_tls,
       tls_verify = config$tls_verify,
       tls_certificates = config$cert)
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
    if (!is.null(type) && string_starts_with(type, "application/json")) {
      msg <- raw_to_json(response$content)$message
    } else {
      ## This is thrown when we send junk to the server -
      ## unmarshalling errors for example.
      msg <- raw_to_char(response$content)
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
http_client_api_version <- function(api_version, ping,
                                    min_version = NULL, max_version = NULL) {
  min_version <- min_version %||% DOCKER_API_VERSION_MIN
  max_version <- max_version %||% DOCKER_API_VERSION_MAX
  version_type <- "Requested"
  if (is.null(api_version)) {
    api_version <- DOCKER_API_VERSION_DEFAULT
  } else if (inherits(api_version, "numeric_version")) {
    assert_scalar(api_version)
    api_version <- as.character(api_version)
  } else {
    assert_scalar_character("api_version")
    if (tolower(api_version) == "detect") {
      api_version <- ping_version(ping())
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


ping_version <- function(res) {
  if (res$status_code != 200L) {
    response_to_error(res, "/_ping", "Detecting version")
  }
  headers <- parse_headers(res$headers)
  names(headers) <- tolower(names(headers))
  if (!("api-version" %in% names(headers))) {
    stop("Failed to detect version.  Headers returned were: ",
         paste(squote(names(headers)), collapse = ""))
  }
  headers[["api-version"]]
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


user_agent_header_string <- function(config) {
  sprintf("stevedore/%s:%s",
          utils::packageVersion("stevedore"), config$http_client_type)
}
