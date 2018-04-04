http_client_httppipe <- function(base_url = NULL, api_version = NULL,
                                 min_version = NULL, max_version = NULL) {
  loadNamespace("httppipe")
  client <- httppipe::httppipe(base_url)
  base_url <- "http://localhost"

  headers_agent <- list("User-Agent" = DEFAULT_USER_AGENT)

  ping <- function() {
    url <- build_url("", DOCKER_API_VERSION_MIN, "/_ping")
    client("GET", url, NULL, headers_agent)
  }
  api_version <- http_client_api_version(api_version, ping,
                                         min_version, max_version)

  request <- function(verb, path, query = NULL, body = NULL, headers = NULL,
                      hijack = NULL) {
    headers <- c(headers_agent, as.list(headers))

    url <- build_url("", api_version, path, query)
    data <- NULL
    if (!is.null(body)) {
      body_data <- prepare_body(body)
      data <- body_data$raw
      headers <- c(headers, list("Content-Type" = body_data$content_type))
    }

    res <- client(verb, url, data, headers, stream = NULL)

    if (!is.null(hijack)) {
      hijack(res$content)
    }
    res
  }

  list(type = "httppipe",
       request = request,
       api_version = api_version,
       can_stream = FALSE,
       ping = ping)
}
