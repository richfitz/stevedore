## NOTE: When updating versions here, update them throughout the
## package - in R/docker_client.R in particular (see the help strings)
MIN_DOCKER_API_VERSION <- "1.25"
MAX_DOCKER_API_VERSION <- "1.33"
DEFAULT_DOCKER_API_VERSION <- "1.29"
DEFAULT_DOCKER_UNIX_SOCKET <- "/var/run/docker.sock"
DEFAULT_DOCKER_WINDOWS_PIPE <- "npipe:////./pipe/docker_engine"
DEFAULT_USER_AGENT <- "stevedore/0.0.0"

.stevedore <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  .stevedore$endpoints <- stevedore_read_endpoints()
  .stevedore$index <- stevedore_read_index()
  .stevedore$spec <- list()
  .stevedore$client_data <- list()
  .stevedore$names <- character()
}
