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


## Support for docker's chunked strings
decode_chunked_string <- function(x, ...) {
  ## This happens when we have logs on a container that has allocated
  ## a tty.  In that case the output is sent directly.  The logic
  ## behind the steps here is that if there are <8 entries then that's
  ## not a complete docker_stream header so don't try and decode it.
  ## And the first 4 elements are an encoded integer with possible
  ## values 0..2 so we _must_ have a raw 0 if this is actually a
  ## docker_stream header.
  if (length(x) < 8L || all(x[seq_len(4)] != 0)) {
    ## NOTE: not 100% sure about splitting the output here but I think
    ## that matches most closely with the docker_stream version where
    ## it is line-based output.  Also not sure if this should be
    ## `\r\n` or `\r?\n` (with fixed = FALSE).
    return(strsplit(raw_to_char(x), "\r\n", fixed = TRUE)[[1L]])
  }

  i_size <- 5L:8L
  to_int <- function(b) {
    ## I don't know if this will work across all platforms (yay,
    ## Solaris) becaus of endianless drama.  But then I doubt that
    ## docker works on Solaris.
    ## packBits(rawToBits(b), "integer")
    sum(256^(3:0) * as.integer(b))
  }

  stream <- integer(0)
  value <- character(0)

  while (length(x) > 0L) {
    len <- to_int(x[i_size])

    stream <- c(stream, x[[1L]])
    value <- c(value, rawToChar(x[9:(len + 8L)]))

    x <- x[-seq_len(len + 8L)]
  }
  docker_stream(value, stream)
}


docker_stream <- function(value, stream) {
  attr(value, "stream") <-
    factor(stream, 0:2, labels = c("stdin", "stdout", "stderr"))
  class(value) <- "docker_stream"
  value
}
