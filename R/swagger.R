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

make_endpoint <- function(method, path, spec, client) {
  force(client)

  x <- spec$paths[[path]][[method]]

  produces <- get_response_type(method, path, x)
  response_handlers <- make_response_handlers(x$responses, spec, produces)

  p <- parse_path(path)

  method <- toupper(method)

  ## TODO: it might make sense to pull client out as a separate
  ## argument, rather than being captured, here?
  list(
    path = path,
    path_args = p$args,
    method = tolower(method),
    response_handlers = response_handlers,
    endpoint = function(path_params, query_params,
                        body = NULL, pass_error = NULL) {
      url <- client$url(sprintfn(p$fmt, path_params), params = query_params)
      res <- client$request2(method, url, body)
      status_code <- res$status_code
      if (status_code < 300) {
        handler <- response_handlers[[as.character(res$status_code)]]
        if (is.null(handler)) {
          stop("unexpected response code ", res$status_code)
        }
        handler(res$content)
      } else if (status_code %in% pass_error) {
        list(status_code = status_code,
             message = response_to_json(res)$message)
      } else {
        response_to_error(res)
      }
    })
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
  schema <- resolve_schema_ref(response, "schema", spec)$schema
  if (schema$type == "object") {
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

make_response_handler_object <- function(schema, spec) {
  ## TODO: there's considerable overlap here with
  ## 'make_response_handler_array_object', though it's not *quite* the
  ## same and I don't know if the code can sensibly be shared.
  els <- names(schema$properties)

  properties <- lapply(schema$properties, resolve_schema_ref2, spec)
  type <- vcapply(properties, "[[", "type")

  atomic <- atomic_types()

  els_atomic <- names(type)[type %in% atomic$names]
  els_array <- names(type)[vlapply(properties, function(x)
    x$type == "array" && x$items %in% atomic$names)]
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
  items <- resolve_schema_ref(schema, "items", spec)$items

  if (items$type == "object") {
    make_response_handler_array_object(items, spec)
  } else {
    message("mmrh_array")
    browser()
  }
}

make_response_handler_array_object <- function(items, spec) {
  cols <- names(items$properties)
  properties <- lapply(items$properties, resolve_schema_ref2, spec)
  type <- vcapply(properties, "[[", "type")

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

make_response_handler_text <- function(...) {
  function(data, convert = TRUE) {
    if (convert) {
      data <- response_text(data)
    }
    data
  }
}

resolve_schema_ref <- function(defn, v, spec) {
  if (identical(names(defn[[v]]), "$ref")) {
    ref <- strsplit(sub("^#/", "", defn[[v]][["$ref"]]), "/",
                    fixed = TRUE)[[1]]
    defn[[v]] <- spec[[ref]]
  }
  defn
}

## This one is probably the one to actually use.
resolve_schema_ref2 <- function(x, spec) {
  if (identical(names(x), "$ref")) {
    ref <- strsplit(sub("^#/", "", x[["$ref"]]), "/",
                    fixed = TRUE)[[1]]
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
