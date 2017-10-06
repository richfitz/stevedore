docker_client <- function(base_url = NULL, api_version = NULL) {
  R6_docker_client$new(base_url, api_version)
}

R6_docker_client <- R6::R6Class(
  "docker_client",
  cloneable = FALSE,
  public = list(
    api = NULL,
    initialize = function(base_url = NULL, api_version = NULL) {
      self$api <- R6_api_client$new(base_url, api_version)
    },
    ping = function() daemon_ping(self$api),
    info = function() daemon_info(self$api),
    df = function() daemon_df(self$api),
    version = function() daemon_version(self$api)
  ))
