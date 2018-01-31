http_client_httppipe <- function(base_url = NULL, api_version = NULL) {
  loadNamespace("httppipe")
  if (!is.null(base_url)) {
    stop("Providing docker url is not currently supported")
  }
  client <- httppipe::httppipe(DEFAULT_DOCKER_UNIX_SOCKET)
  base_url <- "http://localhost"

  headers_agent <- list("User-Agent" = DEFAULT_USER_AGENT)

  version_detect <- function() {
    url <- build_url("", DEFAULT_DOCKER_API_VERSION, "/version")
    version_response(client("GET", url, headers_agent))
  }
  api_version <- http_client_api_version(api_version, version_detect)

  request <- function(verb, path, query = NULL, body = NULL, headers = NULL,
                      hijack = NULL) {
    headers <- c(headers_agent, as.list(headers))

    url <- build_url("", api_version, path, query)
    data <- NULL
    if (!is.null(body)) {
      if (is.raw(body)) {
        data <- body
        content_type <- "application/octet-stream" # or application/x-tar
      } else {
        data <- charToRaw(as.character(body))
        content_type <- "application/json"
      }
      headers <- c(headers, list("Content-Type" = content_type))
    }

    client(verb, url, data, headers, stream = hijack)
  }

  list(type = "httppipe",
       request = request,
       api_version = api_version)
}
