is_docker_error_not_found <- function(e) {
  inherits(e, "docker_error") && e$code == 404L
}


is_error <- function(e) {
  inherits(e, "condition")
}


build_error <- function(message) {
  ret <- list(message = message, call = NULL)
  class(ret) <- c("build_error", "error", "condition")
  ret
}


push_error <- function(message) {
  ret <- list(message = message, call = NULL)
  class(ret) <- c("push_error", "error", "condition")
  ret
}


container_error <- function(container, exit_status, cmd, image, out) {
  err <- out[attr(out, "stream") == "stderr"]
  if (length(err) > 0L) {
    err <- paste0("\n", err, collapse = "")
  } else {
    err <- ""
  }
  msg <- sprintf(
    "Command '%s' in image '%s' returned non-zero exit status %s%s",
    paste(cmd, collapse = " "), image$name(), exit_status, err)
  ret <- list(container = container, exit_status = exit_status,
              cmd = cmd, image = image, out = out, message = msg)
  class(ret) <- c("container_error", "docker_error", "error", "condition")
  ret
}
