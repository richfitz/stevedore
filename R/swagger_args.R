## Autoatic argument handling; read through the swagger spec and
## create a *default* argument handler.  This is going to require
## quite a bit of case-by-case work for nontrivial things.  There's
## also lots of translation required so that we can keep the primarily
## snake-case interface to the package working.

## I think that we'll write these functions to take a client as the
## first argument and then partial that out when creating the
## interface.  That way we can memoise creating the clients by
## version.


## The other way of achiving the same real goal is some sort of
## builder pattern where we take a reciever and a handler and build
## functions with it.  That's probably better actually as it involves
## a bit less straight up magic.

make_argument_handler <- function(method, path, x) {
  ## All the stopifnot bits are assertions that have more to do with
  ## making sure that the spec confirms to what we are expecting.
  ## They'd probably be better done with debugme because I don't think
  ## they should be run by users.

  pars <- x$parameters
  pars_in <- vcapply(pars, "[[", "in")
  pars_optional <- !vlapply(pars, function(x)
    isTRUE(x$required) || x[["in"]] == "body")

  i <- match(pars_in, c("path", "body", "query", "header"))
  stopifnot(all(!is.na(i)))
  ord <- order(pars_optional, i)

  if (!identical(ord, seq_along(ord))) {
    pars_in <- pars_in[ord]
    pars_optional <- pars_optional[ord]
    pars <- pars[ord]
  }

  pars_name <- vcapply(pars, "[[", "name")
  pars_name_r <- pars_name
  pars_name_r[pars_in == "header"] <-
    name_header_to_r(pars_name[pars_in == "header"])
  pars_name_r <- camel_to_snake(pars_name_r)

  if (any(duplicated(pars_name))) {
    stop("fix duplicated names")
  }
  stopifnot(identical(pars_name[pars_in == "path"], parse_path(path)$args))
  stopifnot(sum(pars_in == "body") <= 1L)

  ## These bits here can likely get completely overhauled as there's
  ## heaps of duplication!
  dest <- quote(dest)
  fbody_path <- lapply(pars[pars_in == "path"], arg_collect_path)
  if (length(fbody_path) > 0L) {
    fbody_path <- as.call(c(list(quote(c)), fbody_path))
    fbody_path <- bquote(.(dest)$path <- .(fbody_path))
  }
  fbody_query <- lapply(pars[pars_in == "query"], arg_collect_query, dest)
  if (length(fbody_query) > 0L) {
    fbody_query <- c(bquote(.(dest)$query <- list()), fbody_query)
  }
  if (any(pars_in == "body")) {
    fbody_body <- arg_collect_body(pars[[which(pars_in == "body")]], dest)
    fbody_body <- bquote(.(dest)$body <- .(fbody_body))
  } else {
    fbody_body <- NULL
  }
  fbody_header <- lapply(pars[pars_in == "header"], arg_collect_header, dest)
  if (length(fbody_header) > 0L) {
    fbody_header <- c(bquote(.(dest)$header <- list()), fbody_header)
  }

  fbody <- as.call(c(quote(`{`), bquote(.(dest) <- list()),
                     fbody_path, fbody_query, fbody_body, fbody_header,
                     dest))

  a <- rep(alist(. =, . = NULL), c(sum(!pars_optional), sum(pars_optional)))
  names(a) <- pars_name_r
  env <- parent.env(environment())
  as.function(c(a, fbody), env)
}

arg_collector <- function(p, dest) {
  switch(p[["in"]],
         path = arg_collect_path,
         query = arg_collect_query,
         body = arg_collect_body,
         header = arg_collect_header,
         stop("assertion error"))(p, dest)
}

arg_collect_path <- function(p, dest) {
  if (!isTRUE(p$required)) {
    stop("all path parameters assumed required")
  }
  validate <- quote(assert_scalar_character)
  bquote(.(validate)(.(as.symbol(p$name))))
}

arg_collect_query <- function(p, dest) {
  type <- p$type
  if (type == "boolean") {
    validate <- quote(as_query_logical)
  } else if (type == "integer") {
    validate <- quote(assert_scalar_integer)
  } else if (type == "string") {
    if (grepl("json", p$description, ignore.case = TRUE)) {
      validate <- quote(as_query_json)
    } else {
      validate <- quote(assert_scalar_character)
    }
  } else if (type == "array") {
    if (identical(p$items, list(type = "string"))) {
      validate <- quote(as_query_array_string)
    } else {
      stop("Can't handle this sort of array")
    }
  } else {
    stop("Can't handle this sort of thing")
  }
  nm <- as.symbol(p$name)
  expr <- bquote(.(dest)$query[[.(p$name)]] <- .(validate)(.(nm)))
  if (!isTRUE(p$required)) {
    expr <- bquote(if (!is.null(.(nm))) .(expr))
  }
  expr
}

arg_collect_body <- function(p, dest) {
  ## These ones here pretty much will require a bunch of work; we can
  ## probably outsource a lot of the common bits but all bodies are
  ## nontrivial
  ## POST /containers/create (json data)
  ## POST /conatainers/{id}/update (json data)
  ## PUT /containers/{id}/update (tar string)
  ## POST /build (tar string)
  ## POST /images/create (tar string)
  ## POST /auth (json data)
  ## POST /commit (json data)
  ## POST /images/load (tar string)
  ## POST /containers/{id}/exec (json data)
  ##
  ## For now I can just arrange to pause when I get the data
  expr <- quote(browser())
}

arg_collect_header <- function(p, dest) {
  stopifnot(p$type == "string")
  ## TODO: this does duplicate some argument handling above...
  r_nm <- as.symbol(camel_to_snake(name_header_to_r(p$name)))
  if (is.null(p$enum)) {
    expr <- bquote(assert_scalar_character(.(r_nm)))
  } else {
    values <- as.call(c(quote(c), p$enum))
    expr <- bquote(match_value(.(r_nm), .(values)))
  }
  expr <- bquote(.(dest)$header[[.(p$name)]] <- .(expr))
  if (!isTRUE(p$required)) {
    expr <- bquote(if (!is.null(.(r_nm))) .(expr))
  }
  expr
}

as_query_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar_logical(x, name)
  if (x) "true" else "false"
}

as_query_json <- function(x, name = deparse(substitute(x))) {
  ## TODO: need to convert case here; most cases will actually need
  ## proper handlers I suspect, and these will end up in their own bit
  ## of configuration.
  browser()
}

as_query_array_string <- function(x, name = deparse(substitute(x))) {
  assert_character(x, name)
  paste(x, collapse = ",")
}

as_body <- function(x, name = deparse(substitute(x))) {
  browser()
}

name_header_to_r <- function(x) {
  sub("^x_", "", gsub("-", "_", tolower(x)))
}
