## For now hard code up as the domain socket only; these are
## nontrivial changes required to support more than this!
##
## AIM: all 'curl::' calls go in this file only so that we can decide
## to swap it out for httr later if need be.
##
## This section will hold all the low-level communication bits

handle <- function(base_url, headers = NULL) {
  headers <- c("User-Agent" = DEFAULT_USER_AGENT, headers)
  h <- curl::new_handle()
  curl::handle_setopt(h, UNIX_SOCKET_PATH = base_url)
  curl::handle_setheaders(h, .list = headers)
  h
}

make_handle <- function(base_url) {
  force(base_url)
  function(...) {
    handle(base_url, ...)
  }
}

response_to_error <- function(response) {
  msg <- from_json(response_text(response$content))$message
  code <- response$status_code
  stop(sprintf("%s (code %d)", msg, code))
}

response_to_json <- function(response) {
  from_json(response_text(response$content))
}

response_result <- function(response, as = "text", stop_on_error = TRUE) {
  status_code <- response$status_code
  success <- status_code < 300
  if (stop_on_error && !success) {
    response_to_error(response)
  }
  data <- switch(
    as,
    response = response,
    raw = response$content,
    text = response_text(response$content),
    json = from_json(response_text(response$content)),
    stop("Invalid response type"))
  list(success = success, status_code = status_code, data = data)
}

from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

response_text <- function(bin) {
  ## iconv(readBin(bin, character()), from = "UTF-8", to = "UTF-8")
  rawToChar(bin)
}

R6_api_client <- R6::R6Class(
  "api_client",
  cloneable = FALSE,
  public = list(
    handle = NULL,
    api_version = NULL,
    base_url = NULL,

    initialize = function(base_url = NULL, api_version = NULL) {
      base_url <- base_url %||% DEFAULT_DOCKER_UNIX_SOCKET
      self$handle <- make_handle(base_url)
      self$base_url <- "http://localhost"
      if (is.null(api_version)) {
        api_version <- daemon_version(self, FALSE)$ApiVersion
      }
      if (!identical(api_version, FALSE)) {
        ## TODO: santise this properly
        numeric_version(api_version) # or throw
        self$api_version <- api_version
      }
    },
    GET = function(...) {
      self$request("GET", ...)
    },
    POST = function(...) {
      browser()
      self$request("POST", ...)
    },
    PUT = function(...) {
      browser()
      self$request("PUT", ...)
    },
    DELETE = function(...) {
      browser()
      self$request("DELETE", ...)
    },
    request = function(verb, url, as = "text", stop_on_error = TRUE) {
      h <- self$handle()
      curl::handle_setopt(h, customrequest = verb)
      res <- curl::curl_fetch_memory(url, h)
      response_result(res, as, stop_on_error)
    },
    request2 = function(verb, url, body = NULL) {
      if (!is.null(body)) {
        body_raw <- charToRaw(body)
        h <- self$handle(headers = c("Content-Type" = "application/json"))
        curl::handle_setopt(h,
                            post = TRUE,
                            postfieldsize = length(body_raw),
                            postfields = body_raw,
                            customrequest = verb)
      } else {
        h <- self$handle()
        curl::handle_setopt(h, customrequest = verb)
      }
      curl::curl_fetch_memory(url, h)
    },
    url = function(path, ..., params = NULL, versioned_api = FALSE) {
      v <- if (versioned_api) self$api_version else NULL
      build_url(self$base_url, v, sprintf(path, ...), params)
    }
  ))

build_url <- function(base_url, api_version, path, params = NULL) {
  ## TODO: I think there is some quoting required here! (space -> ' '
  ## etc)
  if (length(params) > 0L) {
    stopifnot(is.list(params),
              !is.null(names(params)),
              all(nzchar(names(params))))
    q <- paste(sprintf("%s=%s", names(params), vcapply(params, identity)),
               collapse = "&")
    path <- sprintf("%s?%s", path, q)
  }
  if (is.null(api_version)) {
    sprintf("%s%s", base_url, path)
  } else {
    sprintf("%s/v%s%s", base_url, api_version, path)
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
