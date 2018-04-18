## may need to pin the SSL version, or allow it to be configured
## https://github.com/docker/docker-py/issues/963

## From the UI perspective, what is the correct behaviour here with
## respect to overriding configuration variables?  I think that the
## right move would be to prefer:
##
## 1. provided answer
## 2. value from environment variable
## 3. platform-specific default
##
## I think that that is consistent with the docker client behaviour.

## Getting behaviour around the DOCKER_TLS_VERIFY correct is not easy,
## partly because it's not really testable.  I think that with the
## proxy approach we can test a plain http version, so that will be
## useful.  For now, just focussing on the main codepaths.

docker_config <- function(api_version = NULL, host = NULL, cert_path = NULL,
                          tls_verify = NULL, http_client_type = NULL,
                          is_windows = NULL, quiet = FALSE) {
  api_version <- api_version %||% Sys_getenv1("DOCKER_API_VERSION")
  host <- host %||% Sys_getenv1("DOCKER_HOST")
  cert_path <- cert_path %||% Sys_getenv1("DOCKER_CERT_PATH")

  docker_config_validate(api_version, host, cert_path, tls_verify,
                         http_client_type, is_windows, quiet)
}

docker_config_validate <- function(api_version, host, cert_path, tls_verify,
                                   http_client_type, is_windows, quiet) {
  assert_scalar_logical(quiet)
  if (!is.null(api_version)) {
    ## validate version here?
  }

  is_windows <- is_windows %||% is_windows()
  if (is.null(host)) {
    if (is_windows) {
      host <- DEFAULT_DOCKER_WINDOWS_PIPE
    } else {
      host <- DEFAULT_DOCKER_UNIX_SOCKET
    }
  }

  host <- trimws(host)
  re <- "^(.+)://(.+)$"
  if (grepl(re, host)) {
    protocol <- sub(re, "\\1", host)
    addr <- sub(re, "\\2", host)
  } else if (grepl("^/", host)) {
    protocol <- "unix"
    addr <- host
  } else {
    stop(sprintf("Invalid address '%s'- must match '<protocol>://<addr>'",
                 host), call. = FALSE)
  }

  if (protocol %in% c("http+unix", "unix")) {
    protocol <- "socket"
    if (is_windows) {
      stop("Socket connections are not possible on windows", call. = FALSE)
    }
    use_tls <- tls_verify <- FALSE
    base_url <- "http://localhost"
  } else if (protocol == "npipe") {
    protocol <- "npipe"
    if (!is_windows) {
      stop("Named pipe connections are only possible on windows",
           call. = FALSE)
    }
    use_tls <- tls_verify <- FALSE
    addr <- sprintf("npipe://%s", addr)
    base_url <- ""
  } else {
    if (protocol == "https") {
      protocol <- "https"
      use_tls <- TRUE
      if (is.null(cert_path)) {
        cert_path <- file.path(path.expand("~"), ".docker")
      }
      tls_verify <- !is.null(tls_verify) && tls_verify
    } else if (protocol %in% c("tcp", "http")) {
      tls_verify <- !is.null(tls_verify) && tls_verify
      use_tls <- tls_verify || !is.null(cert_path)
      protocol <- if (use_tls) "https" else "http"
    } else {
      stop(sprintf("Unknown protocol '%s'", protocol), call. = FALSE)
    }
    base_url <- sprintf("%s://%s", protocol, addr)
  }

  if (is.null(http_client_type)) {
    http_client_type <- if (protocol == "npipe") "httppipe" else "curl"
  } else {
    http_client_type <- match_value(http_client_type,
                                    c("curl", "httppipe", "null"))
  }

  if (protocol == "npipe") {
    if (is_windows) {
      stop("Named pipe connections are only available on windows")
    }
    if (http_client_type == "curl") {
      stop("The 'curl' http driver cannot connect to named pipes")
    }
  }
  if (protocol == "socket" && is_windows) {
    stop("Socket connections are not available on windows")
  }
  if (protocol %in% c("http", "https") && http_client_type == "httppipe") {
    stop("The 'httppipe' http driver cannot connect to http servers")
  }

  if (!is.null(cert_path)) {
    if (protocol %in% c("socket", "npipe")) {
      stop(sprintf("TLS is not supported over '%s' connections", protocol),
           call. = FALSE)
    }
    assert_directory(cert_path)
    req <- c("key.pem", "ca.pem", "cert.pem")
    msg <- req[!file.exists(file.path(cert_path, req))]
    if (length(msg) > 0L) {
      stop(sprintf("Certificate %s missing within directory %s: %s",
                   ngettext(length(msg), "file", "files"),
                   cert_path,
                   paste(squote(msg), collapse = ", ")))
    }
    cert <- list(key = file.path(cert_path, "key.pem"),
                 ca = file.path(cert_path, "ca.pem"),
                 cert = file.path(cert_path, "cert.pem"))
  } else {
    if (tls_verify) {
      stop("cert_path not given, but tls_verify requested")
    }
    cert <- NULL
  }

  list(api_version = api_version,
       protocol = protocol,
       addr = addr,
       base_url = base_url,
       use_tls = use_tls,
       cert = cert,
       tls_verify = tls_verify,
       http_client_type = http_client_type,
       is_windows = is_windows,
       quiet = quiet)
}
