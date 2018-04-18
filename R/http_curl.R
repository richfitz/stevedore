## All curl interactions go in this file; this isolates all the
## low-level communication bits.

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
  function(headers = NULL) {
    headers <- c("User-Agent" = DEFAULT_USER_AGENT, headers)
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
    if (curl::curl_version()$ssl_version == "SecureTransport") {
      opts <- write_p12_cert(opts)
    }
    if (!config$tls_verify) {
      opts$ssl_verifypeer <- FALSE
    }
  } else {
    opts <- NULL
  }
  opts
}


write_p12_cert <- function(opts) {
  loadNamespace("openssl")
  name <- sprintf("%s-docker-key",
                  Sys.getenv("DOCKER_MACHINE_NAME", "stevedore"))
  path <- tempfile(fileext = ".p12")
  opts$keypasswd <- "mypass"
  ## The other way of doing this, without the openssl package, and
  ## probably out of the box happily on mac, is via shell as:
  ##
  ## openssl pkcs12 -export \
  ##         -inkey <cert_path>/key.pem \
  ##         -in <cert_path>/cert.pem \
  ##         -CAfile <cert_path>/ca.pem \
  ##         -chain \
  ##         -name <name> \
  ##         -out <path> \
  ##         -password pass:<keypasswd>
  ##
  ## but for now let's avoid that
  openssl::write_p12(key = opts$sslkey,
                     cert = opts$sslcert,
                     ca = openssl::read_cert_bundle(opts$cainfo),
                     name = name,
                     password = opts$keypasswd,
                     path = path)
  opts$sslcert <- path
  opts
}
