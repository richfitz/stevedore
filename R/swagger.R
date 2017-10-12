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

make_endpoint <- function(path, method, spec, client) {
  force(client)

  x <- spec$paths[[path]][[method]]
  if (!identical(x$produces, "application/json")) {
    stop("non-json endpoints need more work")
  }

  response_handlers <- make_response_handlers(x$responses, spec)
  p <- parse_path(path)

  method <- toupper(method)

  list(
    path = path,
    path_args = p$args,
    method = tolower(method),
    endpoint = function(path_params, query_params) {
      ## TODO: for non-GET methods there is a body to deal with here too.
      url <- client$url(sprintfn(p$fmt, path_params), params = query_params)
      res <- client$request(method, url, as = "response")
      handler <- response_handlers[[as.character(res$status_code)]]
      if (is.null(handler)) {
        stop("unexpected response code ", res$status_code)
      }
      handler(from_json(response_text(res$data$content)))
    })
}

make_response_handlers <- function(responses, spec) {
  lapply(responses[as.integer(names(responses)) < 300],
         make_response_handler, spec)
}

make_response_handler <- function(response, spec) {
  schema <- resolve_schema_ref(response, "schema", spec)$schema
  if (schema$type == "array") {
    make_response_handler_array(schema, spec)
  } else {
    message("make_Response_Handler")
    browser()
    ## if (schema$type == "object") {
    ##   make_response_handler_object(schema, spec)
    ## } else {
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
  opt <- setdiff(cols, items$required)
  if (length(opt)) {
    stop("optional properties not supported")
  }

  type <- vcapply(items$properties, "[[", "type")
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
## parameters (and also the body); otherwise these have to come in as
## json directly which is inconvenient!
##
## process is a function(path, params)
make_reciever <- function(method, path, parameters, client,
                          base = parent.frame()) {
  env <- new.env(parent = base)
  env$client <- client


  path_parts <- parse_path(path)

  if (length(path_parts) == 0) {
    call_path <- path_parts$fmt
  } else {
    call_path <- as.call(c(
      quote(sprintf),
      path_parts$fmt,
      lapply(path_parts$args, as.symbol)))
  }
  if (length(parameters) == 0L) {
    call_parameters <- NULL
  } else {
    call_parameters <- as.call(c(
      quote(list),
      setNames(lapply(parameters, as.symbol), parameters)))
  }
  call_url <- as.call(c(quote(client$url), call_path, params = call_parameters))
  call_method <- as.call(c(quote(`$`),
                           quote(client),
                           as.symbol(toupper(method))))
  call_do <- as.call(c(call_method, quote(url), as = "json"))

  body <- bquote({
    url <- .(call_url)
    res <- .(call_do)

  })

  ## bquote(url <- .(method))

  ## body <- as.call(c(
  ##   quote(`{`),
  ##   bquote(url <- .(call_url))
  ##   quote(

  ##   as.call(c(if (is.symbol(process)) process else quote(process),
  ##             path_format = path_parts$fmt,
  ##             path_parts = call_path,
  ##             parameters = call_parameters))))
  ## dat <- c(setNames(rep(alist(. = ), length(path)), path),
  ##          setNames(rep(alist(. = NULL), length(parameters)), parameters),
  ##          body)

  as.function(dat, env)
}

make_process <- function(client) {
  force(client)
  function(path_format, path_parts, parameters, method) {

    url <- client$build_url(path, parameters[!vlapply(parameters, is.null)])
    dat <- client[[method]](url, as = "json")
  }
}



## make_target <- function(name, method, spec, error, query) {
##   x <- spec$paths[[name]][[method]]
##   if (grepl("{", path, fixed = TRUE)) {

##     browser()
##   }

##   pars <- x$parameters
##   if (!all(vcapply(pars, "[[", "in") == "query")) {
##     stop("handle non query parameters")
##   }

##   default <- lapply(pars, "[[", "default")
##   names(default) <- vcapply(pars, "[[", "name")

##   ## The first step is to make something that does each of the steps
##   ## individually:
##   ##
##   ## * 1. build the payload (for GET that's just the URL)
##   ## * 2. handle the output
##   ##
##   ## Then we pass a client into this and do the whole query

##   nms <- vcapply(pars, "[[", "name")
##   as.expression(setNames(lapply(nms, as.symbol), nms))

##   p <- vector("list", length(pars))
##   names(p) <- vcapply(pars, "[[", "name")
##   body <- bquote({
##     pars <- list(.(x))
##   })
## }

## What we're looking for is some way of converting an
## array-of-objects into a data.frame.  So let's assume that we
## already know that's the situation and work back from there.

## I want to be fairly chill about how much of a full-on swagger
## parser we werite here - I just generally want to simplify how we
## push things in here.

## TODO: naming here needs to be made better - this is more a "result
## handler" and the type is really an "object container".

## TODO: assertions so that this is only applied to array(object)

resolve_schema_ref <- function(defn, v, spec) {
  if (identical(names(defn[[v]]), "$ref")) {
    ref <- strsplit(sub("^#/", "", defn[[v]][["$ref"]]), "/",
                    fixed = TRUE)[[1]]
    defn[[v]] <- spec[[ref]]
  }
  defn
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
