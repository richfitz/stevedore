make_response_handlers <- function(responses, spec, produces) {
  responses <- responses[as.integer(names(responses)) < 300]
  binary_types <- c("application/octet-stream",
                    "application/x-tar",
                    "application/vnd.docker.raw-stream")

  if (produces == "null") {
    lapply(responses, make_response_handler_null, spec)
  } else if (produces == "application/json") {
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
    make_response_handler_null(schema, spec)
  } else if (schema$type == "object") {
    make_response_handler_object(schema, spec)
  } else if (schema$type == "array") {
    make_response_handler_array(schema, spec)
  } else if (schema$type == "string") {
    make_response_handler_string(schema, spec)
  } else {
    stop("not sure how to make this response handler")
  }
}

make_response_handler_null <- function(response, spec) {
  function(data, convert = TRUE) {
    if (length(data) > 0L) {
      stop("Expected an empty response")
    }
    invisible(NULL)
  }
}

make_response_handler_string <- function(schema, spec) {
  as_character <- !identical(schema$format, "binary")
  function(data, convert = TRUE) {
    if (as_character) {
      data <- rawToChar(data)
    }
    data
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
      data <- raw_to_json(data)
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
      data <- raw_to_json(data)
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
      data <- raw_to_char(data)
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

sprintfn <- function(fmt, args) {
  switch(as.character(length(args)),
         "0" = fmt,
         "1" = sprintf(fmt, args),
         "2" = sprintf(fmt, args[[1]], args[[2]]))
}

schema_get_type <- function(x) {
  ret <- x$type
  if (is.null(ret)) {
    ## TODO: this is likely incorrect in some cases, but I suspect
    ## that it's ok most of the time.
    ## TODO; remove this once we roll over to the new resolver
    if ("allOf" %in% names(x)) {
      ret <- "object"
    } else {
      stop("Could not determine type")
    }
  }
  ret
}
