is_docker_error_not_found <- function(e) {
  inherits(e, "docker_error") && e$code == 404L
}
is_error <- function(e) {
  inherits(e, "condition")
}
