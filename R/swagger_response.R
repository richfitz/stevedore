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
make_response_handlers <- function(responses, spec, produces, override) {
  responses <- responses[as.integer(names(responses)) < 300]
  if (!is.null(override)) {
    ## If we need to just override a subset here we should take a list
    ## in rather than a function.
    ret <- rep(list(override), length(responses))
    names(ret) <- names(responses)
    return(ret)
  }

  lapply(responses, make_response_handler, spec, produces)
}

make_response_handler <- function(response, spec, produces) {
  binary_types <- c("application/octet-stream",
                    "application/x-tar",
                    "application/vnd.docker.raw-stream")
  if (produces == "null") {
    make_response_handler_null(response, spec)
  } else if (produces == "application/json") {
    make_response_handler_json(response, spec)
  } else if (produces %in% binary_types) {
    make_response_handler_binary(response)
  } else if (produces == "text/plain") {
    make_response_handler_text(response)
  } else {
    stop("Unhandled response type ", produces) # nocov [stevedore bug]
  }
}

make_response_handler_json <- function(response, spec) {
  schema <- resolve_schema_ref(response$schema, spec)

  if (is.null(schema)) {
    h <- make_response_handler_null(schema, spec)
  } else if (schema$type == "object") {
    h <- make_response_handler_object(schema, spec)
  } else if (schema$type == "array") {
    h <- make_response_handler_array(schema, spec)
  } else if (schema$type == "string") {
    ## This is no longer used here
    h <- make_response_handler_string(schema, spec)
  } else {
    stop("not sure how to make this response handler") # nocov [stevedore bug]
  }
  function(data, as_is_names) {
    h(raw_to_json(data), as_is_names)
  }
}

make_response_handler_null <- function(response, spec) {
  function(data, as_is_names) {
    if (length(data) > 0L) {
      stop("Expected an empty response")
    }
    invisible(NULL)
  }
}

## TODO: there's a big problem here with binary strings - these should
## be treated separately _above_ this function.  We know they're
## coming.
make_response_handler_string <- function(schema, spec) {
  as_character <- !identical(schema$format, "binary")
  function(data, as_is_names) {
    ## if (as_character)) {
    ##   data <- rawToChar(data)
    ## }
    data
  }
}

make_response_handler_object <- function(schema, spec) {
  atomic <- atomic_types()

  additional_properties <- additional_properties_handler <- NULL
  if (!is.null(schema$additionalProperties)) {
    ap <- resolve_schema_ref(schema$additionalProperties, spec)
    if (ap$type == "object") {
      additional_properties <- "object"
      if (!is.null(ap$properties)) {
        additional_properties_handler <- make_response_handler_object(ap, spec)
      }
    } else if (identical(ap, list(type = "string"))) {
      additional_properties <- "string"
    } else {
      stop("Unsupported additionalProperties") # nocov [stevedore bug]
    }
  }

  els <- names(schema$properties)
  els_r <- pascal_to_snake(els)

  if (length(els) == 0 && is.null(additional_properties)) {
    ## This is required for container_stats at least
    additional_properties <- "object"
  }

  properties <- lapply(schema$properties, resolve_schema_ref, spec)
  type <- vcapply(properties, schema_get_type)

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
    make_response_handler_object(x, spec))
  array_handlers <- lapply(properties[els_array], function(x)
    make_response_handler_array(x, spec))

  f_atomic <- function(v, data) {
    pick(data, v, atomic$missing[[type[[v]]]])[[1L]]
  }
  f_object <- function(v, data, as_is_names) {
    x <- pick(data, v, NULL)
    x %&&% object_handlers[[v]](x, as_is_names)
  }
  f_array <- function(v, data, as_is_names) {
    array_handlers[[v]](pick(data, v, NULL), as_is_names)
  }

  function(data, as_is_names) {
    ret <- vector("list", length(type))
    names(ret) <- els
    ret[els_atomic] <- lapply(els_atomic, f_atomic, data)
    ret[els_object] <- lapply(els_object, f_object, data, as_is_names)
    ret[els_array]  <- lapply(els_array,  f_array,  data, as_is_names)
    ## TODO: where we have array strings it might be good to
    ## distinguish between a length 1 array and a string - these have
    ## differecnes in how they're interpreted.  I need to think about
    ## this on the args side too and probably the solution here should
    ## reflect the solution there.  Probably a "scalar" attribute?
    if (!as_is_names && length(ret) > 0L) {
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
          extra <- lapply(extra, additional_properties_handler, as_is_names)
        }
        if (!as_is_names && length(extra) > 0L) {
          names(extra) <- pascal_to_snake(names(extra))
        }
        ret <- c(ret, extra)
      }
    }

    ret
  }
}

make_response_handler_array <- function(schema, spec) {
  items <- resolve_schema_ref(schema$items, spec)
  atomic <- atomic_types()

  if (is.null(items$type)) browser()
  if (items$type == "object") {
    make_response_handler_array_object(items, spec)
  } else if (items$type == "array") {
    make_response_handler_array_array(items, spec)
  } else {
    make_response_handler_array_atomic(atomic$missing[[items$type]],
                                       atomic$empty[[items$type]])
  }
}

make_response_handler_array_atomic <- function(missing, empty) {
  force(missing)
  force(empty)
  function(x, as_is_names) {
    if (is.null(x)) empty else vapply(x, identity, missing, USE.NAMES = FALSE)
  }
}

make_response_handler_array_array <- function(items, spec) {
  handler <- make_response_handler_array(items, spec)
  rm(spec)
  function(x, as_is_names) {
    lapply(x, handler, as_is_names)
  }
}

make_response_handler_array_object <- function(items, spec) {
  if (!is.null(items$additionalProperties)) {
    return(make_response_handler_array_object_list(items, spec))
  } else {
    return(make_response_handler_array_object_df(items, spec))
  }
}

## This one is the nasty case; we handle an object but after
## collection convert it into a data frame.  There's some duplication
## here with make_response_handler_object but it's not avoidable
## because we need to iterate over each element in the array - we're
## basically transposing it but doing some type conversion at the same
## time!
make_response_handler_array_object_df <- function(items, spec) {
  atomic <- atomic_types()
  items$properties <- lapply(items$properties, resolve_schema_ref, spec)
  properties <- lapply(items$properties, resolve_schema_ref, spec)

  cols <- names(properties)
  cols_r <- pascal_to_snake(cols)

  type <- vcapply(properties, "[[", "type")
  stopifnot(all(type %in% c(atomic$names, "object", "array")))

  cols_atomic <- names(type)[type %in% atomic$names]
  cols_object <- names(type)[type == "object"]
  cols_array <- names(type)[type == "array"]

  object_handlers <- lapply(properties[cols_object], function(x)
    make_response_handler_object(x, spec))
  array_handlers <- lapply(properties[cols_array], function(x)
    make_response_handler_array(x, spec))

  f_atomic <- function(v, data) {
    vapply(data, pick, atomic$type[[type[[v]]]], v, atomic$missing[[type[[v]]]],
           USE.NAMES = FALSE)
  }
  f_array <- function(v, data, as_is_names) {
    x <- lapply(data, pick, v, NULL)
    I(lapply(x, array_handlers[[v]], as_is_names))
  }
  f_object <- function(v, data, as_is_names) {
    ## Ah bollocks - I've totally messed this up here and the
    ## recursion is not working correctly I think - the data as coming
    ## in here is missing a layer of listing.  This could come from a
    ## bunch of places unfortunately.
    x <- lapply(data, pick, v, NULL)
    I(lapply(x, object_handlers[[v]], as_is_names))
  }

  function(data, as_is_names) {
    if (!is.null(names(data))) {
      message("Was handed the wrong sort of thing")
      data <- list(data)
    }
    ret <- vector("list", length(cols))
    names(ret) <- cols
    ret[cols_atomic] <- lapply(cols_atomic, f_atomic, data)
    ret[cols_object] <- lapply(cols_object, f_object, data, as_is_names)
    ret[cols_array]  <- lapply(cols_array,  f_array,  data, as_is_names)

    if (!as_is_names && length(ret) > 0L) {
      names(ret) <- cols_r
    }
    as.data.frame(ret, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

make_response_handler_array_object_list <- function(items, spec) {
  properties <- lapply(items$properties, resolve_schema_ref, spec)
  if (length(properties) != 0L) {
    stop("This is not supported") # nocov
  }

  items$additionalProperties <-
    resolve_schema_ref(items$additionalProperties, spec)
  if (identical(items$additionalProperties, list(type = "object"))) {
    additional_properties <- "object"
  } else if (identical(items$additionalProperties, list(type = "string"))) {
    additional_properties <- "string"
  } else {
    stop("Unsupported additionalProperties") # nocov [stevedore bug]
  }

  function(data, as_is_names) {
    if (additional_properties == "string") {
      data <- lapply(data, vcapply, identity)
    } else if (additional_properties != "object") {
      stop("extra handling needed here") # nocov [stevedore bug]
    }
    if (!as_is_names) {
      rename <- function(x) {
        if (!is.null(names(x))) {
          names(x) <- pascal_to_snake(names(x))
        }
        x
      }
      data <- lapply(data, rename)
    }
    data
  }
}

make_response_handler_binary <- function(...) {
  function(data, as_is_names) {
    data
  }
}

make_response_handler_text <- function(...) {
  function(data, as_is_names) {
    raw_to_char(data)
  }
}

make_header_handlers <- function(responses, spec) {
  responses <- responses[as.integer(names(responses)) < 300]
  lapply(responses, make_header_handler, spec)
}

make_header_handler <- function(response, spec) {
  if ("headers" %in% names(response)) {
    els <- names(response$headers)
    els_r <- name_header_to_r(els)
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
    function(headers, as_is_names) {
      h <- parse_headers(headers)
      names(h) <- tolower(names(h))
      ret <- lapply(els, f_atomic, h)
      names(ret) <- if (as_is_names) els else els_r
      ret
    }
  } else {
    NULL
  }
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
    if (!is.null(x$enum)) {
      ret <- "string"
    } else if ("allOf" %in% names(x)) {
      stop("Should not happen") # nocov [stevedore bug]
    } else {
      stop("Could not determine type") # nocov [stevedore bug]
    }
  }
  if (setequal(ret, c("array", "string"))) {
    ret <- "array_string"
  }
  ret
}

## NOTE: dots used here for compatibility with 'convert=' argument;
## this might change later.
decode_chunked_string <- function(x, ...) {
  i_size <- 5L:8L
  to_int <- function(b) {
    sum(256^(3:0) * as.integer(b))
  }

  stream <- integer(0)
  value <- character(0)

  ## TODO: consider dropping newline off here - it's easy enough to
  ## do?  Or probably creating a classed object that we can work with
  ## in a reasonable way.
  while (length(x) > 0L) {
    len <- to_int(x[i_size])

    stream <- c(stream, x[[1L]])
    value <- c(value, rawToChar(x[9:(len + 8L)]))

    x <- x[-seq_len(len + 8L)]
  }
  attr(value, "stream") <-
    factor(stream, 0:2, labels = c("stdin", "stdout", "stderr"))
  class(value) <- "docker_stream"
  value
}

##' @export
format.docker_stream <- function(x, ..., style = "auto",
                                 colour_stdin = "yellow",
                                 colour_stdout = "blue",
                                 colour_stderr = "red",
                                 prefix_stdin = "I< ",
                                 prefix_stdout = "O> ",
                                 prefix_stderr = "E> ") {
  stream <- attr(x, "stream")
  attributes(x) <- NULL
  i_i <- stream == "stdin"
  i_o <- stream == "stdout"
  i_e <- stream == "stderr"
  has_color <- crayon::has_color()
  if (style == "auto") {
    style <- if (has_color) "colour" else "prefix"
  }
  if (style == "plain") {
    x <- x
  } else if (style == "prefix") {
    f <- function(str, prefix) {
      paste0(prefix, gsub("\n(?=.)", paste0("\n", prefix), str, perl = TRUE))
    }
    x[i_i] <- f(x[i_i], prefix_stdin)
    x[i_o] <- f(x[i_o], prefix_stdout)
    x[i_e] <- f(x[i_e], prefix_stderr)
  } else if (style == "colour") {
    x[i_i] <- crayon::style(x[i_i], colour_stdin)
    x[i_o] <- crayon::style(x[i_o], colour_stdout)
    x[i_e] <- crayon::style(x[i_e], colour_stdin)
  }
  x
}

##' @export
print.docker_stream <- function(x, ...) {
  cat(format(x, ...), sep = "")
  invisible(x)
}
