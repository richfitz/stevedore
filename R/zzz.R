DEFAULT_DOCKER_API_VERSION <- "1.29"
DEFAULT_DOCKER_UNIX_SOCKET <- "/var/run/docker.sock"
DEFAULT_USER_AGENT <- "stevedore/0.0.0"

.stevedore <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  .stevedore$endpoints <- stevedore_read_endpoints()
  .stevedore$index <- stevedore_read_index()
  .stevedore$client_data <- list()
}
