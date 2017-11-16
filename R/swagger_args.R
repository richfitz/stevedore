endpoint_args <- function(method, path, x, spec) {
  pars <- x$parameters
  if (is.null(pars)) {
    return(NULL)
  }
  pars_in <- vcapply(pars, "[[", "in")

  is_body <- pars_in == "body"
  if (any(is_body)) {
    stopifnot(sum(is_body) == 1L)
    i_body <- which(is_body)
    body <- pars[[i_body]]
    body$schema <- resolve_schema_ref(body$schema, spec)

    if (body$schema$type == "object") {
      ## TODO; we'll expand this a little if there are more - the idea
      ## is that there is no way of determining from the yaml spec
      ## which parameters are compulsary, so this way we can flag them
      ## one-by-one.  So it's something that will get updated as the
      ## underlying spec changes, but _hopefully_ not too badly with
      ## version and not too often.  Later we can move this
      ## information into the endpoints.yaml file perhaps.  We might
      ## want to arbitrarily patch parameters with additional
      ## information and then process them during argument parsing?
      if (method == "post" && path == "/containers/create") {
        body$schema$properties$Image$required <- TRUE
      }
      to_par <- function(x) {
        c(list(name = x, "in" = "body"), body$schema$properties[[x]])
      }
      pars_body <- lapply(names(body$schema$properties), to_par)

      i1 <- seq_len(i_body - 1L)
      i2 <- setdiff(seq_along(pars), c(i1, i_body))
      pars <- c(pars[i1], pars_body, pars[i2])
      pars_in <- c(pars_in[i1], rep("body", length(pars_body)), pars_in[i2])
      body_type <- "combine"
    } else { # body$schema$type == "string"
      body_type <- "single"
      p <- pars[[i_body]]
      pars[[i_body]] <- c(p[names(p) != "schema"], p$schema)
    }
  } else {
    body_type <- NULL
  }

  pars_name <- vcapply(pars, "[[", "name")
  pars_name_r <- pars_name
  pars_name_r[pars_in == "header"] <-
    name_header_to_r(pars_name[pars_in == "header"])
  pars_name_r <- pascal_to_snake(pars_name_r)
  for (i in seq_along(pars)) {
    pars[[i]]$name_r <- pars_name_r[[i]]
    pars[[i]] <- resolve_schema_ref(pars[[i]], spec)
  }

  if (any(duplicated(pars_name)) || any(duplicated(pars_name_r))) {
    stop("fix duplicated names")
  }
  stopifnot(identical(pars_name[pars_in == "path"], parse_path(path)$args))

  i <- match(pars_in, c("path", "body", "query", "header"))
  stopifnot(all(!is.na(i)))
  pars_req <- vlapply(pars, function(x) isTRUE(x$required))
  pars <- pars[order(!pars_req, i)]

  attr(pars, "body_type") <- body_type

  pars
}

make_argument_handler <- function(method, path, x, spec) {
  ## All the stopifnot bits are assertions that have more to do with
  ## making sure that the spec confirms to what we are expecting.
  ## They'd probably be better done with debugme because I don't think
  ## they should be run by users.

  pars <- endpoint_args(method, path, x, spec)

  ## These bits here can likely get completely overhauled as there's
  ## heaps of duplication!
  dest <- quote(dest)

  body_type <- attr(pars, "body_type")
  if (is.null(body_type)) {
    fbody_body_combine <- NULL
  } else {
    if (body_type == "combine") {
      fbody_body_combine <- as_call(quote(jsonlite::toJSON), dollar(dest, quote(body)))
    } else if (body_type == "single") {
      ## We'd be better off doing this within the core body function
      ## probably but that requires a bit of faff.
      nm <- as.symbol(pars[[which(vcapply(pars, "[[", "in") == "body")]]$name)
      fbody_body_combine <- dollar(dest, quote(body), nm)
    }
    fbody_body_combine <- bquote(.(dollar(dest, quote(body))) <- .(fbody_body_combine))
  }

  fbody_collect <- lapply(pars, arg_collect, dest)
  fbody <- c(quote(`{`),
            bquote(.(dest) <- list()),
            fbody_collect,
            fbody_body_combine,
            dest)

  pars_optional <- !vlapply(pars, function(x) isTRUE(x$required))
  pars_name_r <- vcapply(pars, "[[", "name_r")

  a <- rep(alist(. =, . = NULL), c(sum(!pars_optional), sum(pars_optional)))
  names(a) <- pars_name_r
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
    stop("all path parameters assumed required")
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
      ## message("Skipping validation (array)")
      validate <- quote(identity)
    }
  } else {
    ## message("Skipping validation (other)")
    validate <- quote(identity)
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
    validate <- quote(assert_scalar_character)
    is_scalar <- TRUE
  } else if (type == "array") {
    message("Skipping validation (array)")
    validate <- quote(identity)
    is_scalar <- FALSE
  } else {
    message("Skipping validation (other)")
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
  if (is.null(p$enum)) {
    expr <- bquote(assert_scalar_character(.(nm)))
  } else {
    values <- as_call(quote(c), p$enum)
    expr <- bquote(match_value(.(nm), .(values)))
  }
  expr <- bquote(.(dest)$header[[.(p$name)]] <- .(expr))
  if (!isTRUE(p$required)) {
    expr <- bquote(if (!is.null(.(nm))) .(expr))
  }
  expr
}

as_query_json <- function(x, name = deparse(substitute(x))) {
  ## TODO: need to convert case here; most cases will actually need
  ## proper handlers I suspect, and these will end up in their own bit
  ## of configuration.
  message("as query json")
  browser()
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
