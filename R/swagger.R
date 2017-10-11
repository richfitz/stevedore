make_endpoint <- function(name, method, spec, error, query) {
  x <- spec$paths[[name]][[method]]
  if (grepl("{", path, fixed = TRUE)) {
    browser()
  }

  pars <- x$parameters
  if (!all(vcapply(pars, "[[", "in") == "query")) {
    stop("handle non query parameters")
  }

  default <- lapply(pars, "[[", "default")
  names(default) <- vcapply(pars, "[[", "name")

  ## The first step is to make something that does each of the steps
  ## individually:
  ##
  ## * 1. build the payload (for GET that's just the URL)
  ## * 2. handle the output
  ##
  ## Then we pass a client into this and do the whole query

  nms <- vcapply(pars, "[[", "name")
  as.expression(setNames(lapply(nms, as.symbol), nms))

  p <- vector("list", length(pars))
  names(p) <- vcapply(pars, "[[", "name")
  body <- bquote({
    pars <- list(.(x))
  })
}



## What we're looking for is some way of converting an
## array-of-objects into a data.frame.  So let's assume that we
## already know that's the situation and work back from there.

## I want to be fairly chill about how much of a full-on swagger
## parser we werite here - I just generally want to simplify how we
## push things in here.

## TODO: naming here needs to be made better - this is more a "result
## handler" and the type is really an "object container".

## TODO: assertions so that this is only applied to array(object)

make_parser_container <- function(defn, spec) {
  if (identical(names(defn$items), "$ref")) {
    ref <- strsplit(sub("^#/", "", defn$items[["$ref"]]), "/",
                    fixed = TRUE)[[1]]
    defn$items <- spec[[ref]]
  }

  cols <- names(defn$items$properties)
  opt <- setdiff(cols, defn$items$required)
  if (length(opt)) {
    stop("optional properties not supported")
  }

  type <- vcapply(defn$items$properties, "[[", "type")
  atomic <- list("string" = character(1),
                 "integer" = integer(1))
  cols_atomic <- names(type)[type %in% names(atomic)]
  cols_object <- setdiff(cols, cols_atomic)

  f_atomic <- function(v, data) {
    vapply(data, "[[", atomic[[type[[v]]]], v, USE.NAMES = FALSE)
  }
  f_object <- function(v, data) {
    I(lapply(data, "[[", v))
  }

  function(data) {
    ret <- vector("list", length(type))
    names(ret) <- cols
    ret[cols_atomic] <- lapply(cols_atomic, f_atomic, data)
    ret[cols_object] <- lapply(cols_object, f_object, data)
    ret <- as.data.frame(ret, stringsAsFactors = FALSE)
    ret
  }
}

## This is going to dynamically build up a function out of a set of
## *path* parameters and *query* parameters.  Eventually this needs
## expanding to support body as well and custom handers for different
## parameters (and also the body).
make_reciever <- function(path, parameters, handler, env = NULL) {
  env <- env %||% new.env(parent = environment())
  if (is.function(handler)) {
    env$handler <- handler
  } else if (is.symbol(handler)) {
    if (!exists(as.character(handler), env)) {
      stop("Did not find handler in env")
    }
  }
  call_path <- as.call(c(
    quote(list),
    setNames(lapply(path, as.symbol), path)))
  call_parameters <- as.call(c(
    quote(list),
    setNames(lapply(parameters, as.symbol), parameters)))
  body <- as.call(c(
    quote(`{`),
    as.call(c(if (is.symbol(handler)) handler else quote(handler),
              path = call_path, parameters = call_parameters))))
  dat <- c(setNames(rep(alist(. = ), length(path)), path),
           setNames(rep(alist(. = NULL), length(parameters)), parameters),
           body)
  as.function(dat, env)
}
