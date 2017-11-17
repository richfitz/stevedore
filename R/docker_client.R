docker_client <- function(..., api_version = NULL) {
  cl <- docker_client_base(..., api_version = api_version)

  stevedore_object(
    "docker_client",
    events = strip_api_args("system_events", cl$endpoints),
    df = strip_api_args("system_df", cl$endpoints),
    info = strip_api_args("system_info", cl$endpoints),
    login = strip_api_args("system_auth", cl$endpoints),
    ping = strip_api_args("system_ping", cl$endpoints),
    version = strip_api_args("system_version", cl$endpoints))
}

docker_client_base <- function(..., api_version = NULL) {
  base_url <- NULL
  api_version <- NULL
  self <- new.env(parent = emptyenv())
  self$cl <- R6_http_client$new(base_url, api_version)
  ## I think that we can combine these two a bit?
  dat <- suppressMessages(docker_client_data(self$cl$api_version))
  self$endpoints <- client_endpoints(self$cl, dat$endpoints)
  self
}

stevedore_object <- function(class, ...) {
  els <- list(...)
  nms <- names(els)
  if (is.null(nms) && any(!nzchar(nms)) && any(duplicated(nms))) {
    stop("Invalid names")
  }
  ret <- list2env(els, parent = emptyenv())
  class(ret) <- c(class, "stevedore_object")
  lock_environment(ret)
  ret
}

## There's a certain amount of transformation required here
strip_api_args <- function(name, list) {
  drop_args(list[[name]], c("pass_error", "hijack","as_is_names"),
            name = name)
}
