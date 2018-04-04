http_client_null <- function(base_url, api_version, min_version, max_version) {
  ping <- function() {
    list(status_code = 200,
         headers = charToRaw(paste("Api-Version:", DOCKER_API_VERSION_DEFAULT)),
         content = charToRaw("OK"))
  }
  api_version <- http_client_api_version(api_version, ping,
                                         min_version, max_version)
  request <- function(...) {
    stop("Can't make requests with the null client")
  }
  list(type = "null",
       request = request,
       api_version = api_version,
       can_stream = FALSE,
       ping = request)
}
