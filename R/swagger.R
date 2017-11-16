## Autogenerate some interfaces based on the Swagger/OpenAPI
## specification.

## The simplest way of doing this without going totally spare is
## probably two layers of clients; one is the user-facing one that
## does the argument wrangling.  The other takes a very constant set
## of arguments.  The user facing one will immediately hand off to the
## other.

## There's still a big question here about vectorisation; for example
## things like the equivalent of `docker rmi` will want to be
## vectorised in the image id (but not others).  We can probably
## handle that though.  But I think that we look at sorting out this
## first step and then look at composition above the level of these
## functions.

## From the look of some of the endpoints (e.g., POST
## /containers/create) we might need to shepherd things quite a bit
## more; a generated interface is unlikely to be optimal.

## There are only ~100 endpoints in the current spec and not all will
## be implemented (though there are multiple methods for 97 -> 103).
## But for the simple cases this is going to save a lot of repetitive
## code and automatically allow for differences in the schema over
## time.

make_endpoint <- function(method, path, spec) {
  path_data <- parse_path(path)
  x <- spec$paths[[path]][[method]]
  produces <- get_response_type(method, path, x)

  ## TODO: these will move into the endpoints.yaml file I think
  override <- NULL
  if (method == "get" && path == "/containers/{id}/logs") {
    override <- decode_chunked_string
  }
  response_handlers <-
    make_response_handlers(x$responses, spec, produces, override)

  header_handlers <- make_header_handlers(x$responses, spec)
  argument_handler <- make_argument_handler(method, path, x, spec)

  list(
    path = path,
    path_fmt = path_data$fmt,
    path_args = path_data$args,
    method = toupper(method),
    argument_handler = argument_handler,
    response_handlers = response_handlers,
    header_handlers = header_handlers)
}

run_endpoint <- function(client, endpoint, params,
                         pass_error = NULL, hijack = FALSE,
                         as_is_names = FALSE) {
  path <- sprintfn(endpoint$path_fmt, params$path)
  res <- client$request(endpoint$method, path,
                        params$query, params$body, params$header,
                        hijack)
  if (hijack) {
    return(res)
  }
  status_code <- res$status_code
  if (status_code < 300) {
    r_handler <- endpoint$response_handlers[[as.character(res$status_code)]]
    if (is.null(r_handler)) {
      stop("unexpected response code ", res$status_code)
    }
    ret <- r_handler(res$content, as_is_names = as_is_names)
    h_handler <- endpoint$header_handlers[[as.character(res$status_code)]]
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
  } else {
    response_to_error(res, pass_error)
  }
}

get_response_type <- function(method, path, data) {
  f <- function(x) x$responses[as.integer(names(x$responses)) < 300]
  if (is.null(data)) {
    stop("stevedore bug")
  }
  produces <- data$produces
  if (length(produces) == 0L) {
    responses <- data$responses
    if (any(vlapply(responses[as.integer(names(responses)) < 300],
                    function(x) "schema" %in% names(x)))) {
      ## GET /system/df
      ## GET /containers/{id}/top
      ## GET /containers/{id}/logs
      produces <- "application/json"
      message(sprintf("assuming %s endpoint for %s %s",
                      produces, toupper(method), path))
    } else {
      ## DELETE /networks/{id} & many others
      produces <- "null"
    }
  } else if (length(produces) > 1) {
    browser()
    stop("Multi-output production needs work")
  }
  produces
}


resolve_schema_ref <- function(x, spec) {
  if ("allOf" %in% names(x)) {
    tmp <- lapply(x$allOf, resolve_schema_ref, spec)
    type <- vcapply(tmp, "[[", "type")
    if (!all(type == "object")) {
      stop("work out how to combine non-objects")
    }
    x <- list(type = "object",
              properties = unlist(lapply(tmp, "[[", "properties"), FALSE))
  } else if ("$ref" %in% names(x)) {
    ref <- strsplit(sub("^#/", "", x[["$ref"]]), "/", fixed = TRUE)[[1]]
    x <- c(x[names(x) != "$ref"], resolve_schema_ref(spec[[ref]], spec))
  }
  x
}

parse_path <- function(x) {
  re <- "\\{([^}]+)\\}"
  args <- character(0)
  repeat {
    m <- regexec(re, x)[[1]]
    if (m[[1]] < 0) {
      break
    }
    args <- c(args, substr_len(x, m[[2]], attr(m, "match.length")[[2]]))
    x <- sub(re, "%s", x)
  }

  list(fmt = x, args = args)
}

sprintfn <- function(fmt, args) {
  switch(as.character(length(args)),
         "0" = fmt,
         "1" = sprintf(fmt, args),
         "2" = sprintf(fmt, args[[1]], args[[2]]))
}
