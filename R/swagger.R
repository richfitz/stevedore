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

## some methods will switch output type based on input
##
## as <- switch(x$produces,
##              "application/json" = "json",
##              "text/plain" = "text",
##              "application/octet-stream" = "raw",
##              "application/x-tar" = "raw",
##              "application/vnd.docker.raw-stream" = "response",
##              "raw") # I think

make_endpoint <- function(method, path, spec) {
  message(sprintf("%s %s", toupper(method), path))
  path_data <- parse_path(path)
  x <- spec$paths[[path]][[method]]
  produces <- get_response_type(method, path, x)
  response_handlers <- make_response_handlers(x$responses, spec, produces)
  header_handlers <- make_header_handlers(x$responses, spec)
  endpoint <- make_endpoint_function(path_data, method,
                                     response_handlers,
                                     header_handlers)

  list(
    path = path,
    path_args = path_data$args,
    method = tolower(method),
    response_handlers = response_handlers,
    header_handlers = header_handlers,
    endpoint = endpoint)
}

make_endpoint_function <- function(path_data, method, response_handlers,
                                   header_handlers) {
  method <- toupper(method)
  force(response_handlers)
  force(header_handlers)
  path_fmt <- path_data$fmt

  function(client, path_params, query_params,
           body = NULL, pass_error = NULL) {
    url <- client$url(sprintfn(path_fmt, path_params), params = query_params)
    res <- client$request2(method, url, body)
    status_code <- res$status_code
    if (status_code < 300) {
      r_handler <- response_handlers[[as.character(res$status_code)]]
      if (is.null(r_handler)) {
        stop("unexpected response code ", res$status_code)
      }
      ret <- r_handler(res$content)
      h_handler <- header_handlers[[as.character(res$status_code)]]
      if (!is.null(h_handler)) {
        headers <- h_handler(res$headers)
        if (method == "HEAD") {
          ## There cannot be a body here
          ret <- headers
        } else {
          ret <- set_attributes(ret, headers)
        }
      }
      ret
    } else if (status_code %in% pass_error) {
      list(status_code = status_code,
           message = response_to_json(res)$message)
    } else {
      response_to_error(res)
    }
  }
}

get_response_type <- function(method, path, data) {
  if (is.null(data)) {
    stop("stevedore bug")
  }
  produces <- data$produces
  if (length(produces) == 0L) {
    responses <- data$responses
    if (any(vlapply(responses[as.integer(names(responses)) < 300],
                    function(x) "schema" %in% names(x)))) {
      ## GET /system/df - not sure about others
      produces <- "application/json"
    } else {
      ## DELETE /networks/{id}
      produces <- "text/plain"
    }
    message(sprintf("assuming %s endpoint for %s %s",
                    produces, toupper(method), path))
  } else if (length(produces) > 1) {
    browser()
    stop("Multi-output production needs work")
  }
  produces
}

make_response_handlers <- function(responses, spec, produces) {
  responses <- responses[as.integer(names(responses)) < 300]
  binary_types <- c("application/octet-stream",
                    "application/x-tar")

  if (produces == "application/json") {
    lapply(responses, make_response_handler, spec)
  } else if (produces %in% binary_types) {
    lapply(responses, make_response_handler_binary)
  } else if (produces == "text/plain") {
    lapply(responses, make_response_handler_text)
  } else {
    stop("Unhandled response type ", produces)
  }
}

make_response_handler <- function(response, spec) {
  schema <- resolve_schema_ref(response$schema, spec)

  if (is.null(schema)) {
    make_response_handler_null()
  } else if (schema$type == "object") {
    make_response_handler_object(schema, spec)
  } else if (schema$type == "array") {
    make_response_handler_array(schema, spec)
  } else if (schema$type == "object") {
    make_response_handler_object(schema, spec)
  } else {
    message("make_Response_Handler")
    browser()
    ## if (schema$type == "object") {
    ##   make_response_handler_object(schema, spec)
    ## } else {
  }
}

make_response_handler_null <- function() {
  function(data, convert = TRUE) {
    if (length(data) > 0L) {
      message("Expected an empty response")
      browser()
    }
    invisible(NULL)
  }
}

make_response_handler_object <- function(schema, spec) {
  ## TODO: there's considerable overlap here with
  ## 'make_response_handler_array_object', though it's not *quite* the
  ## same and I don't know if the code can sensibly be shared.
  els <- names(schema$properties)

  properties <- lapply(schema$properties, resolve_schema_ref, spec)
  type <- vcapply(properties, schema_get_type)

  atomic <- atomic_types()

  els_atomic <- names(type)[type %in% atomic$names]
  els_array <- names(type)[vlapply(properties, function(x)
    schema_get_type(x) == "array" && x$items %in% atomic$names)]
  els_object <- setdiff(els, c(els_atomic, els_array))

  ## NOTE: we could filter the use of 'pick' via whether things are
  ## actually optional but I don't think there's much gained there.
  f_atomic <- function(v, data) {
    pick(data, v, atomic$missing[[type[[v]]]])
  }
  f_array <- function(v, data) {
    pick(data, v, NULL) %||% atomic$empty[[properties[[v]]$items$type]]
  }
  f_object <- function(v, data) {
    pick(data, v, NULL)
  }

  function(data, convert = TRUE) {
    if (convert) {
      data <- from_json(response_text(data))
    }
    ret <- vector("list", length(type))
    names(ret) <- els
    ret[els_atomic] <- lapply(els_atomic, f_atomic, data)
    ret[els_object] <- lapply(els_object, f_object, data)
    ret[els_array]  <- lapply(els_array,  f_array,  data)
    ret
  }
}

make_response_handler_array <- function(schema, spec) {
  items <- resolve_schema_ref(schema$items, spec)

  if (items$type == "object") {
    make_response_handler_array_object(items, spec)
  } else {
    message("mmrh_array")
    browser()
  }
}

make_response_handler_array_object <- function(items, spec) {
  cols <- names(items$properties)
  properties <- lapply(items$properties, resolve_schema_ref, spec)
  type <- vcapply(properties, schema_get_type)

  atomic <- atomic_types()

  cols_atomic <- names(type)[type %in% atomic$names]
  cols_array <- names(type)[vlapply(properties, function(x)
    x$type == "array" && x$items %in% atomic$names)]
  cols_object <- setdiff(cols, c(cols_atomic, cols_array))

  ## NOTE: we could filter the use of 'pick' via whether things are
  ## actually optional but I don't think there's much gained there.
  f_atomic <- function(v, data) {
    vapply(data, pick, atomic$type[[type[[v]]]], v, atomic$missing[[type[[v]]]],
           USE.NAMES = FALSE)
  }
  f_array <- function(v, data) {
    ret <- lapply(data, pick, v, NULL)
    i <- lengths(ret) == 0
    if (any(i)) {
      ret[i] <- list(atomic$empty[[properties[[v]]$items$type]])
    }
    I(ret)
  }
  f_object <- function(v, data) {
    I(lapply(data, pick, v, NULL))
  }

  function(data, convert = TRUE) {
    if (convert) {
      data <- from_json(response_text(data))
    }
    ret <- vector("list", length(type))
    names(ret) <- cols
    ret[cols_atomic] <- lapply(cols_atomic, f_atomic, data)
    ret[cols_object] <- lapply(cols_object, f_object, data)
    ret[cols_array]  <- lapply(cols_array,  f_array,  data)
    ret <- as.data.frame(ret, stringsAsFactors = FALSE)
    ret
  }
}

make_response_handler_binary <- function(...) {
  function(data, convert = TRUE) {
    data
  }
}

make_response_handler_header <- function(...) {
  function(data, convert = TRUE) {
  }
}

make_response_handler_text <- function(...) {
  function(data, convert = TRUE) {
    if (convert) {
      data <- response_text(data)
    }
    data
  }
}

make_header_handlers <- function(responses, spec) {
  responses <- responses[as.integer(names(responses)) < 300]
  lapply(responses, make_header_handler, spec)
}

make_header_handler <- function(response, spec) {
  if ("headers" %in% names(response)) {
    els <- names(response$headers)
    type <- vcapply(response$headers, "[[", "type")
    atomic <- atomic_types()
    f_atomic <- function(v, data) {
      msg <- atomic$missing[[type[[v]]]]
      x <- pick(data, tolower(v), msg)
      if (!is.na(x)) {
        storage.mode(x) <- storage.mode(msg)
      }
      x
    }
    function(headers) {
      h <- parse_headers(headers)
      names(h) <- tolower(names(h))
      ret <- lapply(els, f_atomic, h)
      names(ret) <- els
      ret
    }
  } else {
    NULL
  }
}

resolve_schema_ref <- function(x, spec) {
  if (identical(names(x), "$ref")) {
    ref <- strsplit(sub("^#/", "", x[["$ref"]]), "/", fixed = TRUE)[[1]]
    x <- spec[[ref]]
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

atomic_types <- function() {
  type <- list("string"  = character(1),
               "number"  = numeric(1),
               "integer" = integer(1),
               "boolean" = logical(1))
  missing <- lapply(type, as_na)
  empty <- lapply(type, "[", 0L)
  list(names = names(type),
       type = type,
       missing = missing,
       empty = empty)
}

schema_get_type <- function(x) {
  ret <- x$type
  if (is.null(ret)) {
    if ("allOf" %in% names(x)) {
      ret <- "object"
    } else {
      stop("Could not determine type")
    }
  }
  ret
}
