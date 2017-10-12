## This will need tweaking if we allow other version comparisons
api_version_error <- function(required, server) {
  message <- sprintf("API version at least %s required, but server has %s",
                     required, server)
  e <- list(message = message)
  class(e) <- c("api_version_error", "error", "condition")
  e
}
