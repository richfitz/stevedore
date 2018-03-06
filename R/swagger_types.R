swagger_types <- function(version, spec) {
  types <- docker_api_client_types()
  ret <- lapply(types, swagger_type, spec)
  names(ret) <- vcapply(types, "[[", "name")
  ret
}

## Support for complex types (e.g. TaskSpec).  This is a similar
## amount of work to swagger args unfortunately, but might be somewhat
## easier to test.
swagger_type <- function(info, spec) {
  x <- spec[[info$path]]
  handler <- swagger_type_make_handler_object(x, info$name, spec)
  reciever <- swagger_type_make_reciever(x, handler)
  list(name = info$name, handler = handler, reciever = reciever)
}


## TODO: this is going to require, for things like ports, special
## treatment that is similar to how we do these things in the method
## creation.  That's a bit of an annoyance!
swagger_type_make_handler_object <- function(x, typename, spec) {
  stopifnot(x$type == "object")
  force(typename)

  atomic <- atomic_types()

  nms <- names(x$properties)
  nms_r <- pascal_to_snake(nms)
  type <- vcapply(x$properties, "[[", "type")
  is_atomic <- type %in% names(atomic$type)
  is_array <- type == "array"

  array_type <- vcapply(x$properties[is_array], function(x) x$items$type)
  is_array_atomic <- logical(length(is_array))
  is_array_atomic[is_array] <- array_type %in% names(atomic$type)

  ## For now nothing else is handled - object (which we go recursive
  ## with) and arrays of objects (which are going to require some
  ## care)
  stopifnot(all(is_array_atomic | is_atomic))

  handlers <- vector("list", length(nms_r))
  names(handlers) <- nms_r

  handlers[is_atomic] <- Map(swagger_type_make_handler_vector_atomic,
                             nms_r[is_atomic],
                             type[is_atomic])
  handlers[is_array_atomic] <- Map(swagger_type_make_handler_vector_atomic,
                                   nms_r[is_array_atomic],
                                   array_type[is_array_atomic])

  function(data, name = "data") {
    assert_named(data, unique = TRUE, name = name)
    extra <- setdiff(names(data), nms_r)
    if (length(extra) > 0L) {
      stop("Unhandled %s %s",
           ngettext(length(extra), "property", "properties"),
           squote(extra))
    }
    data <- data[!vlapply(data, is.null)]

    for (i in seq_along(data)) {
      data[[i]] <- handlers[[names(data)[[i]]]](data[[i]])
    }

    names(data) <- nms[match(names(data), nms_r)]

    class(data) <- c(typename, "steverdore_type")
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
