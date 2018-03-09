swagger_types <- function(version, spec) {
  types <- docker_api_client_types()

  ret <- set_names(vector("list", length(types)),
                   vcapply(types, "[[", "name"))
  for (t in types) {
    ret[[t$name]] <- swagger_type(t, ret, spec)
  }

  ret
}

## Support for complex types (e.g. TaskSpec).  This is a similar
## amount of work to swagger args unfortunately, but might be somewhat
## easier to test.
swagger_type <- function(info, types, spec) {
  handler <- swagger_type_make_handler_object(info, types, spec)
  reciever <- swagger_type_make_reciever(spec[[info$path]], handler)
  help <- swagger_type_help(spec[[info$path]], info)

  ## NOTE: this name lets us reuse all the formatting bits elsewhere,
  ## even though it's not really quite the right name?
  class(reciever) <- "docker_client_method"
  attr(reciever, "help") <- help

  list(name = info$name, handler = handler, reciever = reciever)
}


## TODO: this is going to require, for things like ports, special
## treatment that is similar to how we do these things in the method
## creation.  That's a bit of an annoyance!
swagger_type_make_handler_object <- function(info, types, spec) {
  typename <- info$name
  x <- spec[[info$path]]

  stopifnot(x$type == "object")

  atomic <- atomic_types()

  properties <- lapply(x$properties, resolve_schema_ref, spec)

  nms <- names(properties)
  nms_r <- pascal_to_snake(nms)
  type <- vcapply(properties, "[[", "type")
  is_atomic <- type %in% names(atomic$type)
  is_array <- type == "array"

  array_type <- rep(NA_character_, length(properties))
  array_type[is_array] <- vcapply(properties[is_array], function(x)
    x$items$type %||% "object")
  is_array_atomic <- logical(length(properties))
  is_array_atomic[is_array] <- array_type[is_array] %in% names(atomic$type)

  is_string_map <- vlapply(properties, object_is_string_map)

  ## For now nothing else is handled - object (which we go recursive
  ## with) and arrays of objects (which are going to require some
  ## care)
  handlers <- vector("list", length(nms_r))
  names(handlers) <- nms_r

  handlers[is_atomic] <- Map(swagger_type_make_handler_vector_atomic,
                             nms_r[is_atomic],
                             type[is_atomic])
  handlers[is_array_atomic] <- Map(swagger_type_make_handler_vector_atomic,
                                   nms_r[is_array_atomic],
                                   array_type[is_array_atomic])
  handlers[is_string_map] <- lapply(nms_r[is_string_map],
                                    swagger_type_make_handler_string_map)

  if (!is.null(info$special)) {
    handlers[names(info$special)] <-
      lapply(info$special, swagger_type_make_handler_subtype, types)
  }

  msg <- lengths(handlers) == 0
  if (any(msg)) {
    message(sprintf("Skipping %d/%d handlers for '%s':\n\t%s",
                    sum(msg), length(msg), typename,
                    paste(squote(nms[msg]), collapse = ", ")))
  }

  function(data, name = "data") {
    if (inherits(data, "stevedore_type")) {
      found <- attr(data, "typename")
      if (!identical(found, typename)) {
        stop("Unexpected input for ", name) # TODO: better message
      }
      return(data)
    }

    assert_named(data, unique = TRUE, name = name)
    extra <- setdiff(names(data), nms_r)
    if (length(extra) > 0L) {
      stop("Unhandled %s %s",
           ngettext(length(extra), "property", "properties"),
           squote(extra))
    }
    data <- data[!vlapply(data, is.null)]

    for (i in seq_along(data)) {
      nm <- names(data)[[i]]
      h <- handlers[[nm]]
      if (is.null(h)) {
        stop(sprintf("Handler for '%s' not yet implemented", nm))
      }
      data[[i]] <- h(data[[i]])
    }

    names(data) <- nms[match(names(data), nms_r)]

    class(data) <- "stevedore_type"
    attr(data, "typename") <- typename

    data
  }
}


swagger_type_make_reciever <- function(x, handler) {
  nms <- names(x$properties)
  nms_r <- pascal_to_snake(nms)

  env <- new_base_env()
  env$.handler <- handler

  args <- rep(alist(a = NULL), length(nms))
  names(args) <- nms_r

  rec <- args
  rec[] <- lapply(nms_r, as.name)
  body <- call(".handler", as.call(c(quote(list), rec)))

  as.function(c(args, body), env)
}


swagger_type_make_handler_scalar_atomic <- function(name, type) {
  force(name)
  validate <- atomic_types()$validate_scalar[[type]]
  stopifnot(is.function(validate))
  function(x) {
    jsonlite::unbox(validate(x, name = name))
  }
}


swagger_type_make_handler_vector_atomic <- function(name, type) {
  force(name)
  validate <- atomic_types()$validate_vector[[type]]
  stopifnot(is.function(validate))
  function(x) {
    validate(x, name = name)
  }
}


swagger_type_make_handler_string_map <- function(name) {
  force(name)
  function(x) {
    if (length(x) == 0L) {
      return(NULL)
    }
    ## I'm pretty sure I've done this already somewhere
    assert_named(x, unique = TRUE)
    if (!is.character(x)) {
      ok <- vlapply(x, function(el) length(el) == 1 && is.character(x))
      stopifnot(all(ok))
    }
    lapply(x, jsonlite::unbox)
  }
}


swagger_type_make_handler_subtype <- function(typename, types) {
  h <- types[[typename]]$handler
  stopifnot(is.function(h))
  function(data, name = "data") {
    h(data, sprintf("%s$%s", name, typename))
  }
}


object_is_string_map <- function(x) {
  x$type == "object" &&
    is.null(x$properties) &&
    identical(x$additionalProperties, list(type = "string"))
}


## TODO: there's some repetition here with above, but that can be
## eliminated later - we'd preprodcess the args list and work with
## that
swagger_type_help <- function(x, info) {
  if (!is.null(info$special)) {
    browser()
  }

  properties <- lapply(x$properties, resolve_schema_ref, spec)
  nms_r <- pascal_to_snake(names(properties))
  args <- set_names(vcapply(properties, pick, "description", NA_character_),
                    nms_r)

  list(name = info$name,
       summary = x$description, # to match method
       args = args)
}
