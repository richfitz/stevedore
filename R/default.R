##' Default docker client.  This contains an active binding
##' (\code{\link{makeActiveBinding}}) to \code{\link{docker_client}} -
##' it can be used as an alternative to calling \code{docker_client()}
##' explicitly (see the examples).  The function
##'
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
##' \item it only works by default if the client construction would
##' work with no arguments.  This is likely to be the case when using
##' a single-machine setup on Linux/macOS with recent docker but is
##' very unlikely on windows.
##'
##' \item Future versions will probably allow setting R's global
##' options to enable fine-tuning of construction (provided this does
##' not complicate the logic in looking up values from environment
##' variables any further).
##'
##' \item The implcitly constructed version may eventually require
##' explicit construction, or a flag to allow implicit construction)
##'
##' }
##'
##' I'd welcome feedback on how useful \code{stevedore::docker} is,
##' but please avoid using it in scripts for now, perhaps.
##'
##' @param ... Parameters passed through to
##'   \code{\link{docker_client}}.  Alternatively a single argument of
##'   a \code{\link{docker_client}} object or \code{NULL} may be given
##'   (if given then no other arguments are allowed).
##' @title Default docker client
##' @export docker
##' @name docker
##' @rdname docker
##' @usage
##' stevedore::docker
##' default_client_set(...)
##' @examples
##'
##' if (docker_available()) {
##'   # This:
##'   stevedore::docker$container$list()
##'
##'   # is roughly equivalent to this:
##'   stevedore::docker_client()$container$list()
##' }
default_client_set <- function(...) {
  dots <- list(...)
  if (length(dots) == 1L && is.null(names(dots)) &&
      (inherits(dots[[1L]], "docker_client") || is.null(dots[[1L]]))) {
    client <- dots[[1L]]
  } else if (length(dots) > 0L && inherits(dots[[1L]], "docker_client")) {
    stop("If setting a default client directly it must be the only argument")
  } else {
    client <- docker_client(...)
  }
  prev <- .stevedore$default_client
  .stevedore$default_client <- client
  invisible(prev)
}


default_client_get <- function() {
  if (is.null(.stevedore$default_client)) {
    message("building client")
    .stevedore$default_client <- docker_client()
  }
  .stevedore$default_client
}


default_client_del <- function() {
  .stevedore$default_client <- NULL
}


f <- function(...) {
  if (length(list(...)) > 0L) {
    list(..1)
  }
}
