## Handlers for the headers are easy because the headers are just
## stringsmapping to fairly simple types - none of these are
## recursive.
swagger_header_handlers <- function(responses, spec) {
  responses <- responses[as.integer(names(responses)) < 300]
  lapply(responses, swagger_header_handler, spec)
}


swagger_header_handler <- function(response, spec) {
  if ("headers" %in% names(response)) {
    els <- names(response$headers)
    els_r <- x_kebab_to_snake(els)
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
    function(headers, output_options) {
      h <- parse_headers(headers)
      names(h) <- tolower(names(h))
      ret <- lapply(els, f_atomic, h)
      names(ret) <- if (output_options$as_is_names) els else els_r
      ret
    }
  } else {
    NULL
  }
}


## This is significantly harder than customising the input; we read
## through the swagger spec and write custom handlers for the json.
## This seems preferable to just relying on the defaults in
## `jsonlite::fromJSON` because things are more likely to be
## type-stable - a zero length array of strings will come out as
## character() and not list(), for example.
##
## There is an existing R package 'rapiclient' for generic swagger
## bindings, but it does not offer hooks that we can use to get just
## the processing part of swagger, and is bound heavily to httr (which
## doesn't work nicely with the unix socket and connection hijacking).
## It also does not handle the spec as provided by docker (it also
## does not have automatically generated handler functions so this
## might be worth pushing upstream).
swagger_response_handlers <- function(responses, spec, produces) {
  responses <- responses[as.integer(names(responses)) < 300]
  lapply(responses, swagger_response_handler, spec, produces)
}


swagger_response_handler <- function(response, spec, produces) {
  binary_types <- c("application/octet-stream",
                    "application/x-tar",
                    "application/vnd.docker.raw-stream")
  if (produces == "null") {
    swagger_response_handler_null(response, spec)
  } else if (produces == "application/json") {
    swagger_response_handler_json(response, spec)
  } else if (produces %in% binary_types) {
    swagger_response_handler_binary(response)
  } else if (produces == "text/plain") {
    swagger_response_handler_text(response)
  } else if (produces == "application/chunked-string") {
    swagger_response_handler_chunked_string(response)
  } else {
    stop("Unhandled response type ", produces) # nocov [stevedore bug]
  }
}


swagger_response_handler_json <- function(response, spec) {
  schema <- resolve_schema_ref(response$schema, spec)

  if (is.null(schema)) {
    h <- swagger_response_handler_null(schema, spec)
  } else if (schema$type == "object") {
    h <- swagger_response_handler_object(schema, spec)
  } else if (schema$type == "array") {
    h <- swagger_response_handler_array(schema, spec)
  } else if (schema$type == "string") {
    h <- swagger_response_handler_json_string(schema, spec)
  } else {
    stop("not sure how to make this response handler") # nocov [stevedore bug]
  }
  function(data, output_options) {
    h(raw_to_json(data), output_options)
  }
}


swagger_response_handler_null <- function(response, spec) {
  function(data, output_options) {
    if (length(data) > 0L) {
      stop("Expected an empty response") # nocov [stevedore bug]
    }
    invisible(NULL)
  }
}


## This is the slightly odd outcome of /swarm/init, which returns a
## json encoded string
swagger_response_handler_json_string <- function(response, spec) {
  function(data, output_options) {
    assert_scalar_character(data)
    data
  }
}


swagger_response_handler_object <- function(schema, spec) {
  atomic <- atomic_types()

  additional_properties <- additional_properties_handler <- NULL
  if (!is.null(schema$additionalProperties)) {
    ap <- resolve_schema_ref(schema$additionalProperties, spec)
    if (ap$type == "object") {
      additional_properties <- "object"
      if (!is.null(ap$properties)) {
        additional_properties_handler <-
          swagger_response_handler_object(ap, spec)
      }
    } else if (ap$type == "array") {
      additional_properties <- "array"
      additional_properties_handler <- swagger_response_handler_array(ap, spec)
    } else if (identical(ap, list(type = "string"))) {
      additional_properties <- "string"
    } else {
      stop("Unsupported additionalProperties") # nocov [stevedore bug]
    }
  }

  els <- names(schema$properties)
  els_r <- pascal_to_snake_cached(els)

  if (length(els) == 0 && is.null(additional_properties)) {
    ## This is required for container_stats at least
    additional_properties <- "object"
  }

  properties <- lapply(schema$properties, resolve_schema_ref, spec)
  type <- vcapply(properties, swagger_get_type)

  is_array_string <- type == "array_string"
  if (any(is_array_string)) {
    for (i in which(is_array_string)) {
      properties[[i]]$type <-
        list(type = "array", items = list(type = "string"))
    }
    type[is_array_string] <- "array"
  }

  ## This is the simplest check now:
  stopifnot(all(type %in% c(atomic$names, "object", "array")))

  els_atomic <- names(type)[type %in% atomic$names]
  els_object <- names(type)[type == "object"]
  els_array <- names(type)[type == "array"]

  ## Quick check here that we found everything ok
  found <- c(els_atomic, els_array, els_object)
  stopifnot(length(found) == length(els) && setequal(found, els))

  object_handlers <- lapply(properties[els_object], function(x)
    swagger_response_handler_object(x, spec))
  array_handlers <- lapply(properties[els_array], function(x)
    swagger_response_handler_array(x, spec))

  f_atomic <- function(v, data) {
    pick(data, v, atomic$missing[[type[[v]]]])[[1L]]
  }
  f_object <- function(v, data, output_options) {
    x <- pick(data, v, NULL)
    x %&&% object_handlers[[v]](x, output_options)
  }
  f_array <- function(v, data, output_options) {
    array_handlers[[v]](pick(data, v, NULL), output_options)
  }

  function(data, output_options) {
    ret <- vector("list", length(type))
    names(ret) <- els
    ret[els_atomic] <- lapply(els_atomic, f_atomic, data)
    ret[els_object] <- lapply(els_object, f_object, data, output_options)
    ret[els_array]  <- lapply(els_array,  f_array,  data, output_options)
    ## TODO: where we have array strings it might be good to
    ## distinguish between a length 1 array and a string - these have
    ## differecnes in how they're interpreted.  I need to think about
    ## this on the args side too and probably the solution here should
    ## reflect the solution there.  Probably a "scalar" attribute?
    if (!output_options$as_is_names && length(ret) > 0L) {
      names(ret) <- els_r
    }

    if (!is.null(additional_properties)) {
      extra <- data[setdiff(names(data), els)]
      if (additional_properties == "string" && length(els) == 0L) {
        ## This deals with the case like the `Options` field in GET
        ## /volumes; it is defined *only* as `AdditionalProperties:
        ## "string"`, in which case it's a named character vector;
        ret <- character(0)
        extra <- vcapply(extra, "[[", 1L)
      }
      if (length(extra) > 0L) {
        if (!is.null(additional_properties_handler)) {
          extra <- lapply(extra, additional_properties_handler, output_options)
        }
        if (!output_options$as_is_names && length(extra) > 0L) {
          names(extra) <- pascal_to_snake_cached(names(extra))
        }
        ret <- c(ret, extra)
      }
    }

    ret
  }
}


swagger_response_handler_array <- function(schema, spec) {
  items <- resolve_schema_ref(schema$items, spec)
  atomic <- atomic_types()

  if (is.null(items$type)) {
    stop("Missing type in items") # nocov [stevedore bug]
  }
  if (items$type == "object") {
    if (!is.null(items$additionalProperties)) {
      swagger_response_handler_array_object_list(items, spec)
    } else {
      swagger_response_handler_array_object_df(items, spec)
    }
  } else if (items$type == "array") {
    swagger_response_handler_array_array(items, spec)
  } else {
    swagger_response_handler_array_atomic(atomic$missing[[items$type]],
                                          atomic$empty[[items$type]])
  }
}


swagger_response_handler_array_atomic <- function(missing, empty) {
  force(missing)
  force(empty)
  function(x, output_options) {
    if (is.null(x)) empty else vapply2(x, identity, missing, USE.NAMES = FALSE)
  }
}


swagger_response_handler_array_array <- function(items, spec) {
  handler <- swagger_response_handler_array(items, spec)
  rm(spec)
  function(x, output_options) {
    lapply(x, handler, output_options)
  }
}


## This one is the nasty case; we handle an object but after
## collection convert it into a data frame.  There's some duplication
## here with swagger_response_handler_object but it's not avoidable
## because we need to iterate over each element in the array - we're
## basically transposing it but doing some type conversion at the same
## time!
swagger_response_handler_array_object_df <- function(items, spec) {
  atomic <- atomic_types()
  items$properties <- lapply(items$properties, resolve_schema_ref, spec)
  properties <- lapply(items$properties, resolve_schema_ref, spec)

  cols <- names(properties)
  cols_r <- pascal_to_snake_cached(cols)

  type <- vcapply(properties, "[[", "type")
  stopifnot(all(type %in% c(atomic$names, "object", "array")))

  cols_atomic <- names(type)[type %in% atomic$names]
  cols_object <- names(type)[type == "object"]
  cols_array <- names(type)[type == "array"]

  object_handlers <- lapply(properties[cols_object], function(x)
    swagger_response_handler_object(x, spec))
  array_handlers <- lapply(properties[cols_array], function(x)
    swagger_response_handler_array(x, spec))

  f_atomic <- function(v, data) {
    vapply2(data, pick, atomic$type[[type[[v]]]], v,
            atomic$missing[[type[[v]]]], USE.NAMES = FALSE)
  }
  f_array <- function(v, data, output_options) {
    x <- lapply(data, pick, v, NULL)
    I(lapply(x, array_handlers[[v]], output_options))
  }
  f_object <- function(v, data, output_options) {
    ## Ah bollocks - I've totally messed this up here and the
    ## recursion is not working correctly I think - the data as coming
    ## in here is missing a layer of listing.  This could come from a
    ## bunch of places unfortunately.
    x <- lapply(data, pick, v, NULL)
    I(lapply(x, object_handlers[[v]], output_options))
  }

  function(data, output_options) {
    if (!is.null(names(data))) {
      stop("Was handed the wrong sort of thing") # nocov [stevedore bug]
    }
    ret <- vector("list", length(cols))
    names(ret) <- cols
    ret[cols_atomic] <- lapply(cols_atomic, f_atomic, data)
    ret[cols_object] <- lapply(cols_object, f_object, data, output_options)
    ret[cols_array]  <- lapply(cols_array,  f_array,  data, output_options)

    if (!output_options$as_is_names && length(ret) > 0L) {
      names(ret) <- cols_r
    }
    output_options$data_frame(
      as.data.frame(ret, stringsAsFactors = FALSE, check.names = FALSE))
  }
}


swagger_response_handler_array_object_list <- function(items, spec) {
  properties <- lapply(items$properties, resolve_schema_ref, spec)
  if (length(properties) != 0L) {
    stop("This is not supported") # nocov
  }

  items$additionalProperties <-
    resolve_schema_ref(items$additionalProperties, spec)
  if (identical(items$additionalProperties, list(type = "string"))) {
    additional_properties <- "string"
  } else {
    stop("Unsupported additionalProperties") # nocov [stevedore bug]
  }

  function(data, output_options) {
    if (additional_properties == "string") {
      data <- lapply(data, vcapply, identity)
    } else {
      stop("extra handling needed here") # nocov [stevedore bug]
    }
    if (!output_options$as_is_names) {
      rename <- function(x) {
        if (!is.null(names(x))) {
          names(x) <- pascal_to_snake_cached(names(x))
        }
        x
      }
      data <- lapply(data, rename)
    }
    data
  }
}


swagger_response_handler_binary <- function(...) {
  function(data, output_options) {
    data
  }
}


swagger_response_handler_text <- function(...) {
  function(data, output_options) {
    raw_to_char(data)
  }
}


swagger_response_handler_chunked_string <- function(...) {
  function(data, output_options) {
    decode_chunked_string(data)
  }
}


## Utility for dealing with swagger types
swagger_get_type <- function(x) {
  ret <- x$type
  if (is.null(ret)) {
    if (!is.null(x$enum)) {
      ret <- "string"
    } else if ("allOf" %in% names(x)) {
      stop("Should not happen [stevedore bug]")
    } else {
      stop("Could not determine type [stevedore bug]")
    }
  }
  if (setequal(ret, c("array", "string"))) {
    ret <- "array_string"
  }
  ret
}
