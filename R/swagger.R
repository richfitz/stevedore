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

stevedore_read_endpoints <- function(reload = FALSE) {
  path <- system.file("spec/endpoints.yaml", package = "stevedore",
                      mustWork = TRUE)
  dat <- yaml_load_file(path)
  for (i in seq_along(dat)) {
    dat[[i]]$name <- names(dat)[[i]]
  }
  unname(dat)
}


make_endpoint <- function(name, method, path, spec) {
  path_data <- parse_path(path)
  x <- spec$paths[[path]][[method]]
  produces <- get_response_type(method, path, x)

  response_handlers <- make_response_handlers(x$responses, spec, produces)

  header_handlers <- make_header_handlers(x$responses, spec)

  response_description <- lapply(x$responses, "[[", "description")

  args <- endpoint_args(method, path, x, spec)
  argument_handler <- make_argument_handler(args)
  help <- get_help(x, args)

  list(
    name = name,
    path = path,
    path_fmt = path_data$fmt,
    path_args = path_data$args,
    method = toupper(method),
    argument_handler = argument_handler,
    response_handlers = response_handlers,
    header_handlers = header_handlers,
    response_description = response_description,
    help = help)
}


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


resolve_schema_ref <- function(x, spec) {
  if ("allOf" %in% names(x)) {
    tmp <- lapply(x$allOf, resolve_schema_ref, spec)
    type <- vcapply(tmp, "[[", "type")
    if (!all(type == "object")) {
      stop("should never happen") # nocov [stevedore bug]
    }
    description <- x$description
    x <- list(type = "object",
              properties = unlist(lapply(tmp, "[[", "properties"), FALSE))
    x$description <- description
  } else if ("$ref" %in% names(x)) {
    ref <- strsplit(sub("^#/", "", x[["$ref"]]), "/", fixed = TRUE)[[1]]
    x <- c(x[names(x) != "$ref"], resolve_schema_ref(spec[[ref]], spec))
  }
  x
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

hijacked_content <- function(hijack) {
  if (!is.null(attr(hijack, "content"))) {
    attr(hijack, "content")()
  }
}

get_help <- function(x, args) {
  if (length(args) == 0L) {
    args <- NULL
  } else {
    args <- set_names(vcapply(args, pick, "description", NA_character_),
                      vcapply(args, "[[", "name_r"))
  }
  list(summary = x$summary, description = x$description, args = args)
}
