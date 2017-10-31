## For now hard code up as the domain socket only; these are
## nontrivial changes required to support more than this!
##
## All 'curl::' calls go in this file; this isolates all the low-level
## communication bits.  The only major overlap is that we also do a
## version check here before we start using the generated api.

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
      self$api_version <- client_api_version(api_version, self)
    },
    request = function(verb, url, body = NULL, headers = NULL,
                        hijack = FALSE, mode = "rb") {
      if (!is.null(body)) {
        body_raw <- charToRaw(body)
        h <- self$handle(headers = c("Content-Type" = "application/json",
                                     headers))
        curl::handle_setopt(h,
                            post = TRUE,
                            postfieldsize = length(body_raw),
                            postfields = body_raw,
                            customrequest = verb)
      } else {
        h <- self$handle(headers = headers)
        curl::handle_setopt(h, customrequest = verb)
      }
      if (verb == "HEAD") {
        curl::handle_setopt(h, nobody = TRUE)
      }
      if (hijack) {
        con <- curl::curl(url, handle = h)
        open(con, mode = mode, blocking = FALSE)
        data <- curl::handle_data(h)
        data$connection <- data
        data
      } else {
        curl::curl_fetch_memory(url, h)
      }
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

client_api_version <- function(api_version, client) {
  if (is.null(api_version)) {
    api_version <- DEFAULT_DOCKER_API_VERSION
  } else if (inherits(api_version, "numeric_version")) {
    assert_scalar(api_version)
    api_version <- as.character(api_version)
  } else {
    assert_scalar_character("api_version")
    if (tolower(api_version) == "detect") {
      url <- client$url('/version', versioned_api = FALSE)
      res <- client$request("GET", url)
      api_version <- response_result(res, "json")$data$ApiVersion
    } else {
      numeric_version(api_version) # or throw
    }
  }
  api_version
}
