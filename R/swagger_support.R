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


## Parse the parametrised paths used in the swagger spec,
## e.g. /containers/{id}/logs into an sprintf format and a set of
## names ("/containers/%s/logs" and "id").
swagger_path_parse <- function(x) {
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
