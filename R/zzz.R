## NOTE: When updating versions here, update them throughout the
## package - in R/docker_client.R in particular (see the help strings)
DOCKER_API_VERSION_MIN <- "1.25"
DOCKER_API_VERSION_MAX <- "1.37"
DOCKER_API_VERSION_DEFAULT <- "1.29"

DEFAULT_DOCKER_UNIX_SOCKET <- "unix:///var/run/docker.sock"
DEFAULT_DOCKER_WINDOWS_PIPE <- "npipe:////./pipe/docker_engine"
DEFAULT_USER_AGENT <- "stevedore/0.0.0"

HELP <- "\r\b:\n"

.stevedore <- new.env(parent = emptyenv())

stevedore_reset <- function() {
  rm(list = ls(.stevedore, all.names = TRUE), envir = .stevedore)
  pascal_to_snake_cache_reset()
  .stevedore$spec <- list()
  .stevedore$client_data <- list()
  .stevedore$curl_uses_secure_transport <- curl_uses_secure_transport()
}

.onLoad <- function(...) {
  stevedore_reset()
}
