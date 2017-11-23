## All 'curl::' calls go in this file; this isolates all the low-level
## communication bits.  The only major overlap is that we also do a
## version check here before we start using the generated api.
R6_http_client <- R6::R6Class(
  "http_client",
  cloneable = FALSE,
  public = list(
    handle = NULL,
    api_version = NULL,
    base_url = NULL,

    initialize = function(base_url = NULL, api_version = NULL) {
      ## For now hard code up as the domain socket only.  Changing
      ## this to support working over a port is slightly more work;
      ## we'll need to change the initialiser and the
      ## make_handle_socket function and change the base_url
      ## (currently it is set to http://localhost, which is not what
      ## would be wanted if we had a proper url).
      if (!is.null(base_url)) {
        stop("Providing docker url is not currently supported")
      }
      base_url <- base_url %||% DEFAULT_DOCKER_UNIX_SOCKET
      self$handle <- make_handle_socket(base_url)
      self$base_url <- "http://localhost"
      max_version <- max(numeric_version(names(.stevedore$index)))
      self$api_version <-
        http_client_api_version(api_version, self,
                                MIN_DOCKER_API_VERSION,
                                MAX_DOCKER_API_VERSION)
    },

    request = function(verb, path, query = NULL, body = NULL, headers = NULL,
                       hijack = FALSE, mode = "rb", versioned_api = TRUE) {
      v <- if (versioned_api) self$api_version else NULL
      url <- build_url(self$base_url, v, path, query)
      if (!is.null(body)) {
        if (is.raw(body)) {
          body_raw <- body
          content_type <- "application/octet-stream" # or application/x-tar
        } else {
          body_raw <- charToRaw(body)
          content_type <- "application/json"
        }
        h <- self$handle(headers = c("Content-Type" = content_type, headers))
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
    }
  ))

## Factory function for fresh curl handles.  In theory we could do
## this from a pool but for now I'm just doing this in the most
## obvious way I can think of.
make_handle_socket <- function(base_url) {
  force(base_url)
  function(headers = NULL) {
    headers <- c("User-Agent" = DEFAULT_USER_AGENT, headers)
    h <- curl::new_handle()
    curl::handle_setopt(h, UNIX_SOCKET_PATH = base_url)
    curl::handle_setheaders(h, .list = headers)
    h
  }
}

## Generate (and possibly throw) S3 errors out of http errors
response_to_error <- function(response, pass_error) {
  headers <- curl::parse_headers_list(response$headers)
  type <- headers[["content-type"]]
  if (length(response$content) > 0L) {
    if (type == "text/plain") {
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
  cond <- list(message = msg, code = status_code)
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

## TODO: through here (and again through the package code that calls
## this) we need to be able to pin down the maxmimum api version - the
## swagger spec is published only up to 1.32 but the mac version
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
http_client_api_version <- function(api_version, client,
                                    min_version, max_version) {
  version_type <- "Requested"
  if (is.null(api_version)) {
    api_version <- DEFAULT_DOCKER_API_VERSION
  } else if (inherits(api_version, "numeric_version")) {
    assert_scalar(api_version)
    api_version <- as.character(api_version)
  } else {
    assert_scalar_character("api_version")
    if (tolower(api_version) == "detect") {
      version_type <- "Detected"
      ## TODO; temporarily set the version here to the default version
      ## (i.e., whatever we ship with) to avoid hitting the deprecated
      ## api-without-version issue.
      res <- client$request("GET", "/version", versioned_api = FALSE)
      api_version <- raw_to_json(res$content)$ApiVersion
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
