## Standalone functions for dealing with docker's data
short_id <- function(x) {
  end <- if (string_starts_with(x, "sha256:")) 17L else 10L
  substr(x, 1, end)
}


drop_leading_slash <- function(x) {
  sub("^/", "", x)
}


## TODO: this should be renamed to make it clear it is adding "latest"
## and should go via parse_image_name
image_name <- function(x, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  ## TODO: run this through parse_image_name?  This will fail on repos
  ## with a port, for example.
  if (!grepl(":", x, fixed = TRUE)) {
    x <- paste0(x, ":latest")
  }
  x
}


## TODO: this does not handle references (repo/image@ref) but that's
## not that hard to add in here provided we can actually pass the @ref
## through as if it was tag to things like pull
DOCKER_REPO_RE <- '^(.+/)?([^:]+)(:[^:]+)?$'
parse_image_name <- function(image, name = deparse(substitute(image))) {
  assert_scalar_character(image, name)
  if (!grepl(DOCKER_REPO_RE, image, perl = TRUE)) {
    stop(sprintf("'%s' does not match pattern '[<repo>/]<image>[:<tag>]'",
                 image))
  }
  name <- sub(DOCKER_REPO_RE, "\\2", image, perl = TRUE)

  repo <- sub(DOCKER_REPO_RE, "\\1", image)
  if (nzchar(repo)) {
    repo <- sub("/$", "", repo)
  } else {
    repo <- NULL
  }

  ## The SHA processing here might be wrong.  The RE might need to
  ## allow a leading 'sha265:' I think!
  tag <- sub(DOCKER_REPO_RE, "\\3", image)
  if (nzchar(tag)) {
    tag <- sub("^:", "", tag)
  } else {
    tag <- NULL
  }

  ## This is actually the name that we want to use:
  image <- if (is.null(repo)) name else paste(repo, name, sep = "/")
  list(repo = repo, name = name, image = image, tag = tag)
}
