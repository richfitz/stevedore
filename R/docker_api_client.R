docker_api_client <- function(base_url = NULL, api_version = NULL,
                              type = NULL) {
  self <- new.env(parent = parent.env(environment()))
  self$http_client <- http_client(base_url, api_version, type)
  self$endpoints <- docker_api_client_data(self$http_client$api_version)
  self$api_version <- self$http_client$api_version
  lock_environment(self)
  self
}


docker_api_client_data <- function(version) {
  if (!(version %in% names(.stevedore$client_data))) {
    spec <- swagger_spec_read(version)
    endpoints <- docker_api_client_endpoints(version)
    docker_api_client_data_check(spec, endpoints)

    dat <- lapply(endpoints, function(x)
      swagger_endpoint(x$name, x$method, x$path, x$from, spec))
    names(dat) <- vcapply(endpoints, "[[", "name")
    attr(dat, "version") <- version

    .stevedore$client_data[[version]] <- dat
  }
  .stevedore$client_data[[version]]
}


docker_api_client_data_check <- function(spec, endpoints) {
  done <- vcapply(endpoints, function(x) paste(x$method, x$path))

  pos <- lapply(spec$paths, names)
  pos <- paste(unlist(pos, FALSE, FALSE), rep(names(spec$paths), lengths(pos)))

  ## Ignore missing "POST /session" because it is experimental
  unk <- setdiff(done, pos)
  msg <- setdiff(pos, c(done, "post /session"))

  if (length(unk) > 0L) {
    stop("Unknown endpoints (stevedore bug):\n",
            paste("  -", unk, collapse = "\n"))
  }
  if (length(msg) > 0L) {
    message("Unimplemented endpoints (stevedore bug):\n",
            paste("  -", msg, collapse = "\n"))
  }
}


docker_api_client_endpoints <- function(version) {
  if (is.null(.stevedore$endpoints)) {
    path <- stevedore_file("spec/endpoints.yaml")
    dat <- yaml_load_file(path)
    for (i in seq_along(dat)) {
      dat[[i]]$name <- names(dat)[[i]]
    }
    .stevedore$endpoints <- unname(dat)
  }

  ret <- .stevedore$endpoints
  version <- numeric_version(version)
  ret <- ret[vlapply(ret, function(x) version_at_least(version, x$from))]
  ret
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
