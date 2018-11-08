##' Default docker client.  This contains an active binding
##' (\code{\link{makeActiveBinding}}) to \code{\link{docker_client}} -
##' it can be used as an alternative to calling \code{docker_client()}
##' explicitly (see the examples).  This is a convenience function
##' designed primarily for interactive use - see Details below.
##'
##' \code{default_client_set} can be used to control the creation of a
##' specific default client.  It returns (invisibly) the previous
##' default client (\code{NULL} if none was set) so can be used
##' similarly to \code{par}, \code{options} etc, with \code{on.exit}.
##'
##' @section Warnings:
##'
##' This interface is experimental and may change.  In particular:
##'
##' \enumerate{
##'
##' \item the implicit \code{stevedore::docker} object only if the
##' client construction would work with no arguments.  This is likely
##' to be the case when using a single-machine setup on Linux/macOS
##' with recent docker but is very unlikely on windows (particularly
##' on Windows 7 with docker via docker toolbox/machine).
##'
##' \item Future versions will probably allow setting R's global
##' options to enable fine-tuning of construction (provided this does
##' not complicate the logic in looking up values from environment
##' variables any further).
##'
##' \item The implicitly constructed version may eventually require
##' explicit construction, or a flag to allow implicit construction)
##'
##' }
##'
##' I'd welcome feedback on how useful \code{stevedore::docker} is,
##' but perhaps avoid using it in scripts for now.
##'
##' @param ... Parameters passed through to
##'   \code{\link{docker_client}}.  Alternatively a single argument of
##'   a \code{\link{docker_client}} object or \code{NULL} may be given
##'   (if given then no other arguments are allowed).
##' @inheritParams docker_client
##' @title Default docker client
##' @export docker
##' @export default_client_set
##' @name docker
##' @rdname docker
##' @examples
##'
##' if (stevedore::docker_available()) {
##'   # This:
##'   stevedore::docker$container$list()
##'
##'   # is roughly equivalent to this:
##'   client <- stevedore::docker_client()
##'   client$container$list()
##'
##'   # To control the api version used by the default client, you can use:
##'   stevedore::docker$api_version()
##'   p <- stevedore::default_client_set(api_version = "1.35")
##'   stevedore::docker$api_version()
##'   # Revert to the previous version:
##'   stevedore::default_client_set(p)
##' }
default_client_set <- function(..., quiet = TRUE) {
  dots <- list(...)
  if (length(dots) == 1L && is.null(names(dots)) &&
      (inherits(dots[[1L]], "docker_client") || is.null(dots[[1L]]))) {
    client <- dots[[1L]]
  } else if (length(dots) > 0L && inherits(dots[[1L]], "docker_client")) {
    stop("If setting a default client directly it must be the only argument")
  } else {
    client <- docker_client(..., quiet = quiet)
  }
  prev <- .stevedore$default_client
  .stevedore$default_client <- client
  invisible(prev)
}


default_client_get <- function() {
  if (is.null(.stevedore$default_client)) {
    default_client_set()
  }
  .stevedore$default_client
}


default_client_del <- function() {
  .stevedore$default_client <- NULL
}


default_client_binding <- function(env) {
  if (!exists("docker", env)) {
    makeActiveBinding("docker", default_client_get, env)
  }
}
