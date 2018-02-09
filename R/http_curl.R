## All curl interactions go in this file; this isolates all the
## low-level communication bits.

## The only other curl:: call is in http where we use it's header
## parsing.
http_client_curl <- function(base_url = NULL, api_version = NULL,
                             min_version = NULL, max_version = NULL) {
  ## For now hard code up as the domain socket only.  Changing
  ## this to support working over a port is slightly more work;
  ## we'll need to change the initialiser and the
  ## make_handle_socket function and change the base_url
  ## (currently it is set to http://localhost, which is not what
  ## would be wanted if we had a proper url).
  if (http_url_type(base_url) == "http") {
    stop("Providing docker http/https url is not currently supported")
  }

  version_detect <- function() {
    url <- build_url(base_url, DEFAULT_DOCKER_API_VERSION, "/version")
    version_response(curl::curl_fetch_memory(url, handle()))
  }

  base_url <- base_url %||% DEFAULT_DOCKER_UNIX_SOCKET
  handle <- make_handle_socket(base_url)
  base_url <- "http://localhost"
  api_version <- http_client_api_version(api_version, version_detect,
                                         min_version, max_version)

  request <- function(verb, path, query = NULL, body = NULL, headers = NULL,
                      hijack = NULL) {
    url <- build_url(base_url, api_version, path, query)
    if (!is.null(body)) {
      body_data <- prepare_body(body)
      h <- handle(headers = c("Content-Type" = body_data$content_type, headers))
      curl::handle_setopt(h,
                          post = TRUE,
                          postfieldsize = length(body_data$raw),
                          postfields = body_data$raw,
                          customrequest = verb)
    } else {
      h <- handle(headers = headers)
      curl::handle_setopt(h, customrequest = verb)
    }
    if (verb == "HEAD") {
      curl::handle_setopt(h, nobody = TRUE)
    }
    if (!is.null(hijack)) {
      assert_is(hijack, "function")
      ## NOTE: if I need to use a connection (e.g., to write to stdin)
      ## then curl::handle_data is the way to get the information out
      ## of the headers.  curl::curl is the same as
      ## curl:::curl_collection (see the body of curl_fetch_stream.
      ## Jeroen has certainly made this nice to work with!
      curl::curl_fetch_stream(url, hijack, h)
    } else {
      curl::curl_fetch_memory(url, h)
    }
  }

  list(type = "curl",
       request = request,
       api_version = api_version,
       can_stream = TRUE)
}

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
