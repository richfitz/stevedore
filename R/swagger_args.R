endpoint_args <- function(method, path, x, spec) {
  args <- x$parameters
  if (is.null(args)) {
    return(NULL)
  }
  args_in <- vcapply(args, "[[", "in")

  is_body <- args_in == "body"
  if (any(is_body)) {
    stopifnot(sum(is_body) == 1L)
    i_body <- which(is_body)
    body <- args[[i_body]]
    body$schema <- resolve_schema_ref(body$schema, spec)

    if (body$schema$type == "object") {
      description <- tolower1(body$description) %||% "request body"
      to_par <- function(x) {
        el <- resolve_schema_ref(body$schema$properties[[x]], spec)
        el$description <- el$description %||% paste("For", description)
        c(list(name = x, "in" = "body"), el)
      }
      args_body <- lapply(names(body$schema$properties), to_par)

      i1 <- seq_len(i_body - 1L)
      i2 <- setdiff(seq_along(args), c(i1, i_body))
      args <- c(args[i1], args_body, args[i2])
      args_in <- c(args_in[i1], rep("body", length(args_body)), args_in[i2])
      body_type <- "combine"
    } else { # body$schema$type == "string"
      body_type <- "single"
      p <- args[[i_body]]
      args[[i_body]] <- c(p[names(p) != "schema"], p$schema)
    }
  } else {
    body_type <- NULL
  }

  args_name <- vcapply(args, "[[", "name")
  args_name_r <- args_name
  args_name_r[args_in == "header"] <-
    name_header_to_r(args_name[args_in == "header"])
  args_name_r <- pascal_to_snake(args_name_r)
  for (i in seq_along(args)) {
    args[[i]]$name_r <- args_name_r[[i]]
    args[[i]] <- resolve_schema_ref(args[[i]], spec)
  }

  if (any(duplicated(args_name)) || any(duplicated(args_name_r))) {
    stop("fix duplicated names") # nocov [stevedore bug]
  }
  stopifnot(identical(args_name[args_in == "path"], parse_path(path)$args))

  i <- match(args_in, c("path", "body", "query", "header"))
  stopifnot(all(!is.na(i)))
  args_req <- vlapply(args, function(x) isTRUE(x$required))
  args <- args[order(!args_req, i)]

  attr(args, "body_type") <- body_type

  args
}

make_argument_handler <- function(args) {
  ## All the stopifnot bits are assertions that have more to do with
  ## making sure that the spec confirms to what we are expecting.
  ## They'd probably be better done with debugme because I don't think
  ## they should be run by users.
  dest <- quote(dest)

  body_type <- attr(args, "body_type")
  if (is.null(body_type)) {
    fbody_body_combine <- NULL
  } else {
    if (body_type == "combine") {
      fbody_body_combine <-
        as_call(quote(jsonlite::toJSON), dollar(dest, quote(body)))
    } else if (body_type == "single") {
      ## We'd be better off doing this within the core body function
      ## probably but that requires a bit of faff.
      nm <- as.symbol(args[[which(vcapply(args, "[[", "in") == "body")]]$name)
      fbody_body_combine <- dollar(dest, quote(body), nm)
    }
    fbody_body_combine <- bquote(
      .(dollar(dest, quote(body))) <- .(fbody_body_combine))
  }

  fbody_collect <- lapply(args, arg_collect, dest)
  fbody <- c(quote(`{`),
            bquote(.(dest) <- list()),
            fbody_collect,
            fbody_body_combine,
            dest)

  args_optional <- !vlapply(args, function(x) isTRUE(x$required))
  args_name_r <- vcapply(args, "[[", "name_r")

  a <- rep(alist(. =, . = NULL), c(sum(!args_optional), sum(args_optional)))
  names(a) <- args_name_r
  env <- parent.env(environment())
  as.function(c(a, as.call(fbody)), env)
}

arg_collect <- function(p, dest) {
  switch(p[["in"]],
         path = arg_collect_path(p, dest),
         query = arg_collect_query(p, dest),
         body = arg_collect_body(p, dest),
         header = arg_collect_header(p, dest),
         stop("assertion error"))
}

arg_collect_path <- function(p, dest) {
  if (!isTRUE(p$required)) {
    stop("all path parameters assumed required") # nocov [stevedore bug]
  }
  rhs <- as_call(quote(assert_scalar_character), as.symbol(p$name_r))
  lhs <- dollar(dest, quote(path), as.symbol(p$name))
  as_call(quote(`<-`), lhs, rhs)
}

## some of the 'query' bits within here must change - we might need to
## construct different validators depending on what sort of input
## we're getting?  It might be better to realise that avoiding
## duplication here is just making this function worse, not better!
arg_collect_query <- function(p, dest) {
  type <- p$type
  stopifnot(length(type) == 1L)
  if (type == "boolean") {
    validate <- quote(assert_scalar_logical)
  } else if (type == "integer") {
    validate <- quote(assert_scalar_integer)
  } else if (type == "string") {
    validate <- quote(assert_scalar_character)
  } else if (type == "array") {
    if (identical(p$items, list(type = "string"))) {
      validate <- quote(as_query_array_string)
    } else {
      stop("Unknown query type") # nocov [stevedore bug]
    }
  } else {
    stop("Unknown query type") # nocov [stevedore bug]
  }

  nm <- as.symbol(p$name)
  nm_r <- as.symbol(p$name_r)
  rhs <- as_call(validate, nm_r)
  lhs <- dollar(dest, quote(query), nm)
  expr <- as_call(quote(`<-`), lhs, rhs)
  if (!isTRUE(p$required)) {
    expr <- bquote(if (!is.null(.(nm_r))) .(expr))
  }
  expr
}

## This is really similar to above but not *that* similar really -
## when combined they're clumsy and hard to reason about.
arg_collect_body <- function(p, dest) {
  type <- p$type
  if (setequal(type, c("array", "string"))) {
    is_scalar <- FALSE
    validate <- quote(as_body_array_string)
  } else if (type == "boolean") {
    validate <- quote(assert_scalar_logical)
    is_scalar <- TRUE
  } else if (type == "integer") {
    validate <- quote(assert_scalar_integer)
    is_scalar <- TRUE
  } else if (type == "string") {
    if (identical(p$format, "binary")) {
      validate <- quote(assert_raw)
      is_scalar <- FALSE
    } else {
      validate <- quote(assert_scalar_character)
      is_scalar <- TRUE
    }
  } else if (type == "array") {
    ## message("Skipping validation (array)") # TODO
    validate <- quote(identity)
    is_scalar <- FALSE
  } else {
    ## message("Skipping validation (other)") # TODO
    validate <- quote(identity)
    is_scalar <- FALSE
  }

  nm <- as.symbol(p$name)
  nm_r <- as.symbol(p$name_r)
  rhs <- as_call(validate, nm_r)
  if (is_scalar) {
    rhs <- as_call(quote(jsonlite::unbox), rhs)
  }
  lhs <- dollar(dest, quote(body), nm)
  expr <- as_call(quote(`<-`), lhs, rhs)
  if (!isTRUE(p$required)) {
    expr <- bquote(if (!is.null(.(nm_r))) .(expr))
  }
  expr
}

arg_collect_header <- function(p, dest) {
  stopifnot(p$type == "string")
  nm <- p$name_r
  sym <- as.name(nm)
  is_required <- !isTRUE(p$required)
  has_default <- !is.null(p$default)

  if (is.null(p$enum)) {
    expr <- bquote(assert_scalar_character(.(nm)))
  } else {
    values <- as_call(quote(c), p$enum)
    expr <- bquote(match_value(.(sym), .(values)))
  }
  if (is_required && has_default) {
    expr <- bquote(if (is.null(.(sym))) .(p$default) else .(expr))
  }
  expr <- bquote(.(dest)$header[[.(p$name)]] <- .(expr))
  if (!is_required) {
    expr <- bquote(if (!is.null(.(nm))) .(expr))
  }
  expr
}

as_query_array_string <- function(x, name = deparse(substitute(x))) {
  assert_character(x, name)
  paste(x, collapse = ",")
}

as_body_array_string <- function(x, name = deparse(substitute(x))) {
  assert_character(x, name)
  if (inherits(x, "AsIs")) {
    assert_scalar(x, name)
    jsonlite::unbox(x)
  } else {
    x
  }
}

name_header_to_r <- function(x) {
  sub("^x_", "", gsub("-", "_", tolower(x)))
}
