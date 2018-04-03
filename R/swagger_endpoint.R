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
swagger_endpoint <- function(x, types, spec) {
  if (!is.null(x$from) && !version_at_least(spec$info$version, x$from)) {
    return(swagger_endpoint_unsupported(x, spec$info$version))
  }
  path_data <- swagger_path_parse(x$path)
  d <- spec$paths[[x$path]][[x$method]]
  produces <- get_response_type(x$method, x$path, d)

  response_handlers <- swagger_response_handlers(d$responses, spec, produces)
  header_handlers <- swagger_header_handlers(d$responses, spec)

  response_description <- lapply(d$responses, "[[", "description")

  args <- swagger_args(x$method, x$path, d, x$handlers, types, spec)
  argument_handler <- args$handler
  help <- args$help
  help$cli <- x$cli

  if (is.null(x$extra)) {
    extra <- NULL
  } else {
    extra_default <- function(el) {
      if (!("default" %in% names(el))) {
        alist(a = )[[1L]]
      } else if (is.null(el$default)) {
        NULL
      } else {
        parse(text = el$default, n = 1L, keep.source = FALSE)[[1L]]
      }
    }
    help$args[names(x$extra)] <- lapply(x$extra, "[[", "help")
    extra <- lapply(x$extra, extra_default)
  }

  list(
    name = x$name,
    path = x$path,
    path_fmt = path_data$fmt,
    method = toupper(x$method),
    argument_handler = argument_handler,
    response_handlers = response_handlers,
    extra = extra,
    header_handlers = header_handlers,
    response_description = response_description,
    help = help)
}


## Basically just used above
get_response_type <- function(method, path, data) {
  f <- function(x) x$responses[as.integer(names(x$responses)) < 300]
  if (is.null(data)) {
    stop("stevedore bug") # nocov [stevedore bug]
  }
  produces <- data$produces
  if (length(produces) == 0L) {
    responses <- data$responses
    if (any(vlapply(responses[as.integer(names(responses)) < 300],
                    function(x) "schema" %in% names(x)))) {
      ## This is now considered a bug: patch the spec explicitly instead
      stop(sprintf("Can't determine type for %s %s", method, path)) # nocov
    } else {
      ## DELETE /networks/{id} & many others
      produces <- "null"
    }
  } else if (length(produces) > 1) {
    stop("Multi-output production needs work") # nocov [stevedore bug]
  }
  produces
}


swagger_endpoint_unsupported <- function(x, version_used) {
  list(name = x$name,
       path = x$path,
       method = toupper(x$method),
       version_required = x$from,
       version_used = version_used,
       unsupported = TRUE)
}
