http_client_null <- function(base_url, api_version, min_version, max_version) {
  version_detect <- function() {
    DEFAULT_DOCKER_API_VERSION
  }
  api_version <- http_client_api_version(api_version, version_detect,
                                         min_version, max_version)
  request <- function(...) {
    stop("Can't make requests with the null client")
  }
  list(type = "null",
       request = request,
       api_version = api_version,
       can_stream = FALSE)
}
