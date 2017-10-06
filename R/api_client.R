## For now hard code up as the domain socket only; these are
## nontrivial changes required to support more than this!
##
## AIM: all 'curl::' calls go in this file only so that we can decide
## to swap it out for httr later if need be.
##
## This section will hold all the low-level communication bits

handle <- function(base_url) {
  h <- curl::new_handle()
  curl::handle_setopt(h, UNIX_SOCKET_PATH = base_url)
  curl::handle_setheaders(h, c("User-Agent" = DEFAULT_USER_AGENT))
  h
}

response_result <- function(response, as = "text", stop_on_error = TRUE) {
  if (stop_on_error && response$status_code >= 300) {
    stop("Error code ", response$status_code)
  }
  switch(
    as,
    response = response,
    raw = response$content,
    text = response_text(response$content),
    json = from_json(response_text(response$content)),
    stop("Invalid response type"))
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
      self$handle <- handle(base_url)
      self$api_version <- version %||% DEFAULT_DOCKER_API_VERSION
      self$base_url <- "http://localhost"
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
      curl::handle_setopt(self$handle, customrequest = verb)
      on.exit(curl::handle_reset(self$handle))
      res <- curl::curl_fetch_memory(url, self$handle)
      response_result(res, as, stop_on_error)
    },
    url = function(..., versioned_api = FALSE) {
      ## TODO: Quote the query fragment of a URL; replacing ' ' with '+'
      path <- paste(c(...), collapse = "/")
      if (versioned_api) {
        sprintf("%s/v%s%s", self$base_url, self$api_version, path)
      } else {
        sprintf("%s%s", self$base_url, path)
      }
    }
  ))
