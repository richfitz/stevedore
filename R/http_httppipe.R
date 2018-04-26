http_client_httppipe <- function(config,
                                 min_version = NULL, max_version = NULL) {
  client <- httppipe(config$addr)
  ## TODO - this should use config$base_url but that needs changes in
  ## the httppipe package
  base_url <- ""

  user_agent <- user_agent_header_string(config)

  ping <- function() {
    url <- build_url(base_url, DOCKER_API_VERSION_MIN, "/_ping")
    client("GET", url, NULL, list("User-Agent" = user_agent))
  }
  api_version <- http_client_api_version(config$api_version, ping,
                                         min_version, max_version)

  request <- function(verb, path, query = NULL, body = NULL, headers = NULL,
                      hijack = NULL) {
    headers <- c(list("User-Agent" = user_agent), as.list(headers))

    url <- build_url(base_url, api_version, path, query)
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
