## All curl interactions go in this file; this isolates all the
## low-level communication bits.
##
## The only other curl:: call is in http where we use it's header
## parsing.
http_client_curl <- function(config, min_version = NULL, max_version = NULL) {
  handle <- make_curl_handle(config)
  base_url <- config$base_url

  ping <- function() {
    url <- build_url(base_url, DOCKER_API_VERSION_MIN, "/_ping")
    curl::curl_fetch_memory(url, handle())
  }
  api_version <- http_client_api_version(config$api_version, ping,
                                         min_version, max_version)

  request <- function(verb, path, query = NULL, body = NULL, headers = NULL,
                      hijack = NULL) {
    url <- build_url(base_url, api_version, path, query)
    if (!is.null(body)) {
      body_data <- prepare_body(body)
      h <- handle(headers = c("Content-Type" = body_data$content_type, headers))
      curl::handle_setopt(h,
                          post = TRUE,
                          postfieldsize = length(body_data$raw),
                          postfields = body_data$raw,
                          customrequest = verb)
    } else {
      h <- handle(headers = headers)
      curl::handle_setopt(h, customrequest = verb)
    }
    if (verb == "HEAD") {
      curl::handle_setopt(h, nobody = TRUE)
    }
    if (!is.null(hijack)) {
      assert_is(hijack, "function")
      ## NOTE: if I need to use a connection (e.g., to write to stdin)
      ## then curl::handle_data is the way to get the information out
      ## of the headers.  curl::curl is the same as
      ## curl:::curl_collection (see the body of curl_fetch_stream.
      ## Jeroen has certainly made this nice to work with!
      curl::curl_fetch_stream(url, hijack, h)
    } else {
      curl::curl_fetch_memory(url, h)
    }
  }

  list(type = "curl",
       request = request,
       api_version = api_version,
       can_stream = TRUE,
       ping = ping)
}


make_curl_handle <- function(config) {
  opts <- curl_handle_opts(config)
  user_agent <- user_agent_header_string(config)
  function(headers = NULL) {
    headers <- c("User-Agent" = user_agent, headers)
    h <- curl::new_handle()
    curl::handle_setopt(h, .list = opts)
    curl::handle_setheaders(h, .list = headers)
    h
  }
}


curl_handle_opts <- function(config) {
  if (config$protocol == "socket") {
    opts <- list(UNIX_SOCKET_PATH = config$addr)
  } else if (config$use_tls) {
    opts <- list(sslkey = config$cert$key,
                 cainfo = config$cert$ca,
                 sslcert = config$cert$cert)
    ## This is only required on MacOS I believe
    if (.stevedore$curl_uses_secure_transport) {
      opts <- curl_handle_opts_secure_transport(opts)
    }
    if (!config$tls_verify) {
      opts$ssl_verifypeer <- FALSE
    }
  } else {
    opts <- NULL
  }
  if (!is.null(config$debug)) {
    opts$verbose <- TRUE
    opts$debugfunction <- curl_debugfunction(config$debug)
  }
  opts
}


curl_handle_opts_secure_transport <- function(opts) {
  name <- sprintf("%s-docker-key",
                  Sys.getenv("DOCKER_MACHINE_NAME", "stevedore"))
  opts$keypasswd <- "mypass"
  opts$sslcert <-
    write_p12(opts$sslkey, opts$cainfo, opts$sslcert, name, opts$keypasswd)
  opts
}


curl_uses_secure_transport <- function() {
  curl::curl_version()$ssl_version == "SecureTransport"
}


curl_debugfunction <- function(stream = stdout()) {
  ## Debug types are:
  ##
  ##   0: text
  ##   1: header in
  ##   2: header out
  ##   3: data in
  ##   4: data out
  ##   5: ssl in
  ##   6: ssl out
  ##
  ## These are the close to the prefixes used by httr, which seems
  ## like a reasonable place to start.
  prefix <- list(" *", "<-", "->", "<<", ">>", "*<", "*>")
  ## There's a bit of a faff here involved with managing to use
  ## capture.output that I don't see is necessary but is really
  ## required for the tests.
  is_stdout <- identical(stream, stdout())
  function(type, msg) {
    p <- prefix[[type + 1L]]
    if (any(msg == as.raw(0x00))) {
      txt <- sprintf("%s <binary %d bytes>", p, length(msg))
    } else {
      msg_lines <- unlist(strsplit(rawToChar(msg), "[\r\n]+"))
      txt <- paste(sprintf("%s %s\n", p, msg_lines), collapse = "")
    }
    cat(txt, file = if (is_stdout) stdout() else stream)
  }
}


## Can do this via either the system or the openssl package:
write_p12 <- function(key, ca, cert, name, password, openssl_pkg = NULL) {
  path <- tempfile("stevedore_p12_", fileext = ".p12")

  openssl_pkg <- openssl_pkg %||% requireNamespace("openssl", quietly = TRUE)
  if (openssl_pkg) {
    ca <- openssl::read_cert_bundle(ca)
    openssl::write_p12(key = key, cert = cert, ca = ca,
                       name = name, password = password, path = path)
  } else {
    args <- c("pkcs12", "-export", "-inkey", key, "-in", cert, "-CAfile", ca,
              "-chain", "-name", name, "-out", path, "-password",
              paste0("pass:", password))
    system3(Sys_which("openssl"), args)
  }

  path
}
