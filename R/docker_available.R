##' Test if we can construct a docker client and confirm that we can
##' communicate with it.  This is intended to help in debug connection
##' issues, and also for use in tests.  For example, you might
##' implement a testthat skip test that skips if
##' \code{stevedore::docker_available()} returns \code{FALSE} to
##' conditionally use stevedore/docker within tests.
##'
##' Reasons for failure to connect might include:
##'
##' \itemize{
##' \item You do not have a docker daemon running
##'
##' \item You have docker installed but the socket in a nonstandard
##' place and have not adjusted environment variables accordingly
##'
##' \item You do not have permission to write to the docker socket
##'
##' \item You are on windows and the required python packages to get
##'   everything working there are not present or configured correctly
##'
##' \item There are problems arranging verification over https/tls.
##' }
##'
##' If \code{versose} is \code{TRUE} then some diagnostic information
##' will be printed.
##'
##' @title Test if docker available
##'
##' @param verbose Logical, indicating if information should be
##'   printed about failures to connect.  If \code{FALSE} (the
##'   default) the function runs silently.
##'
##' @param ... Passed through to \code{docker_client} (e.g.,
##'   \code{api_version}, \code{host}).
##'
##' @return Logical scalar, \code{TRUE} if
##'   \code{\link{docker_client}(...)} would succeed.
##'
##' @export
##' @examples
##' # Is docker available on your system?
##' stevedore::docker_available()
docker_available <- function(..., verbose = FALSE) {
  ## NOTE: Despite what this says above, this does not actually call
  ## `docker_client` because that's quite slow.  Instead we call
  ## http_client, which is way faster and does not require internet
  ## access to get the spec.
  res <- tryCatch({
    cl <- http_client(docker_config(...))
    cl$ping()
  }, error = identity)

  ok <- !inherits(res, "error")
  if (verbose && !ok) {
    what <- if (!exists("cl", inherits = FALSE))
              "create docker client" else "connect to docker daemon"
    message(sprintf("Failed to %s with error message:\n  %s",
                    what, res$message))
  }
  ok
}
