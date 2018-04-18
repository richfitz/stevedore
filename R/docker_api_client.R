docker_api_client <- function(config) {
  self <- new.env(parent = parent.env(environment()))
  self$http_client <- http_client(config)

  data <- docker_api_client_data(self$http_client$api_version, config$quiet)
  self$endpoints <- data$endpoints
  self$types <- data$types

  self$api_version <- self$http_client$api_version
  self$auth <- docker_api_client_auth()
  lock_environment(self)
  self
}


docker_api_client_data <- function(version, quiet = FALSE) {
  if (!(version %in% names(.stevedore$client_data))) {
    spec <- swagger_spec_read(version, quiet)
    endpoints <- docker_api_client_endpoints()
    docker_api_client_data_check(spec, endpoints)

    dat <- list()

    dat$types <- swagger_types(version, spec)

    dat$endpoints <- lapply(endpoints, function(x)
      swagger_endpoint(x, dat$types, spec))
    names(dat$endpoints) <- vcapply(endpoints, "[[", "name")

    dat$version <- version

    .stevedore$client_data[[version]] <- dat
  }
  .stevedore$client_data[[version]]
}


docker_api_client_help <- function(class, name) {
  if (is.null(.stevedore$help)) {
    .stevedore$help <- yaml_load_file(stevedore_file("spec/help.yaml"))
  }
  ret <- .stevedore$help[[class]][[name]]
  ret$name <- name
  ret
}


## endpoints that need this:
##   POST /images/create
##   POST /images/{name}/push
##   POST /build
##   some plugin ones not yet handled
docker_api_client_auth <- function() {
  data <- new.env()
  list(
    set = function(serveraddress, value) {
      data[[serveraddress]] <- base64encode(as.character(value))
    },
    get = function(serveraddress) {
      data[[serveraddress]]
    })
}


STEVEDORE_UNIMPLEMENTED <- "post /session"


docker_api_client_data_check <- function(spec, endpoints) {
  done <- vcapply(endpoints, function(x) paste(x$method, x$path))

  pos <- lapply(spec$paths, names)
  pos <- paste(unlist(pos, FALSE, FALSE), rep(names(spec$paths), lengths(pos)))
  pos_check <- setdiff(pos, STEVEDORE_UNIMPLEMENTED)

  msg <- setdiff(pos_check, done)
  if (length(msg) > 0L) {
    stop("Unimplemented endpoints (stevedore bug):\n",
         paste("  -", msg, collapse = "\n"))
  }

  err <- intersect(done, STEVEDORE_UNIMPLEMENTED)
  if (length(err) > 0L) {
    stop("Unexpected implemented methods (stevedore bug):\n",
         paste("  -", err, collapse = "\n"))
  }
}


docker_api_client_endpoints <- function() {
  if (is.null(.stevedore$endpoints)) {
    path <- stevedore_file("spec/endpoints.yaml")
    dat <- yaml_load_file(path)
    for (i in seq_along(dat)) {
      dat[[i]]$name <- names(dat)[[i]]
    }
    .stevedore$endpoints <- unname(dat)
  }
  .stevedore$endpoints
}


docker_api_client_types <- function() {
  if (is.null(.stevedore$types)) {
    path <- stevedore_file("spec/types.yaml")
    dat <- yaml_load_file(path)
    for (i in seq_along(dat)) {
      dat[[i]]$name <- names(dat)[[i]]
    }
    .stevedore$types <- unname(dat)
  }
  .stevedore$types
}


run_endpoint <- function(http_client, endpoint, params, hijack = NULL,
                         allow_hijack_without_stream = FALSE,
                         as_is_names = FALSE) {
  path <- sprintfn(endpoint$path_fmt, params$path)

  http_hijack <- !is.null(hijack)
  if (http_hijack && !http_client$can_stream && !allow_hijack_without_stream) {
    fmt <- paste(
      "Endpoint '%s' cannot be implemented because the '%s' http client",
      "does not currently support streaming connections")
    stop(sprintf(fmt, endpoint$name, http_client$type))
  }

  res <- http_client$request(endpoint$method, path,
                             params$query, params$body, params$header,
                             hijack)
  if (http_hijack) {
    res$content <- hijacked_content(hijack)
  }

  status_code <- res$status_code
  if (status_code >= 300) {
    reason <-
      endpoint$response_description[[as.character(status_code)]] %||%
      "Unknown reason"
     response_to_error(res, endpoint$name, reason)
  } else {
    r_handler <- endpoint$response_handlers[[as.character(res$status_code)]]
    if (is.null(r_handler)) {
      stop("unexpected response code ", res$status_code) # nocov [stevedore bug]
    }
    h_handler <- endpoint$header_handlers[[as.character(res$status_code)]]
    if (http_hijack) {
      ## It's most common here that the any handler is incorrect, so
      ## we'll skip the handler but pass them back directly instead.
      list(response = res,
           content_handler = r_handler,
           header_handler = h_handler)
    } else {
      ret <- r_handler(res$content, as_is_names = as_is_names)
      if (!is.null(h_handler)) {
        headers <- h_handler(res$headers, as_is_names = as_is_names)
        if (endpoint$method == "HEAD") {
          ## There cannot be a body here
          ret <- headers
        } else {
          ret <- set_attributes(ret, headers)
        }
      }
      ret
    }
  }
}


hijacked_content <- function(hijack) {
  if (!is.null(attr(hijack, "content"))) {
    attr(hijack, "content")()
  }
}
