context("http with curl")


test_that("construction", {
  skip_on_windows()
  skip_if_not_using_docker()

  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "curl",
                          is_windows = FALSE)
  cl <- http_client_curl(config)

  expect_is(cl, "list")
  expect_equal(cl$type, "curl")
  expect_is(cl$request, "function")
  expect_true(cl$can_stream)
  expect_is(cl$ping, "function")

  version_api <- parse_headers(rawToChar(cl$ping()$headers))[["Api-Version"]]
  version_max <- DOCKER_API_VERSION_MAX
  version_expected <- as.character(
    min(numeric_version(version_api), numeric_version(version_max)))

  expect_equal(cl$api_version, version_expected)
})


test_that("options: socket", {
  cfg <- docker_config(ignore_environment = TRUE, is_windows = FALSE)
  expect_equal(
    curl_handle_opts(cfg),
    list(UNIX_SOCKET_PATH = sub("unix://", "", DEFAULT_DOCKER_UNIX_SOCKET)))
})


test_that("options: https", {
  .stevedore$curl_uses_secure_transport <- FALSE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())

  tls_path <- fake_tls_dir()
  on.exit(unlink(tls_path, recursive = TRUE), add = TRUE)
  cfg <- docker_config(ignore_environment = TRUE,
                       host = "https://1.2.3.4:5678",
                       cert_path = tls_path,
                       tls_verify = TRUE)
  expect_equal(
    curl_handle_opts(cfg),
    list(sslkey = file.path(tls_path, "key.pem"),
         cainfo = file.path(tls_path, "ca.pem"),
         sslcert = file.path(tls_path, "cert.pem")))

  cfg <- docker_config(ignore_environment = TRUE,
                       host = "https://1.2.3.4:5678",
                       cert_path = tls_path,
                       tls_verify = FALSE)
  expect_equal(
    curl_handle_opts(cfg),
    list(sslkey = file.path(tls_path, "key.pem"),
         cainfo = file.path(tls_path, "ca.pem"),
         sslcert = file.path(tls_path, "cert.pem"),
         ssl_verifypeer = FALSE))
})


test_that("write p12", {
  skip_if_not_installed("openssl")
  path <- write_p12("tls/key.pem", "tls/ca.pem", "tls/cert.pem",
                    "stevedore-test-p12", "mypass")
  expect_true(file.exists(path))
  expect_match(path, "\\.p12$")
  on.exit(unlink(path))

  cmp <- openssl::read_p12("tls/cert.p12", "mypass")

  expect_error(openssl::read_p12(path, "anotherpass"))
  expect_silent(dat <- openssl::read_p12(path, "mypass"))
  expect_equal(dat, cmp)
})


test_that("options: https + secure transport", {
  .stevedore$curl_uses_secure_transport <- TRUE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())

  cfg <- docker_config(ignore_environment = TRUE,
                       host = "https://1.2.3.4:5678",
                       cert_path = "tls",
                       tls_verify = TRUE)
  opts <- curl_handle_opts(cfg)

  expect_equal(opts$sslkey, "tls/key.pem")
  expect_equal(opts$cainfo, "tls/ca.pem")
  expect_match(opts$sslcert, "\\.p12$")
  expect_true(file.exists(opts$sslcert))
  expect_equal(opts$keypasswd, "mypass")

  unlink(opts$sslcert)
})


test_that("options: http", {
  .stevedore$curl_uses_secure_transport <- FALSE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())

  cfg <- docker_config(ignore_environment = TRUE,
                       host = "http://1.2.3.4:5678")
  expect_null(curl_handle_opts(cfg))
})


test_that("make handle", {
  .stevedore$curl_uses_secure_transport <- FALSE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())
  cfg <- docker_config(ignore_environment = TRUE,
                       host = "https://1.2.3.4:5678",
                       cert_path = "tls",
                       tls_verify = TRUE)
  factory <- make_curl_handle(cfg)
  expect_is(factory, "function")

  expect_silent(h <- factory())
  expect_is(h, "curl_handle")
  ## no public function for getting current options
})


test_that("connect over http", {
  cl <- test_docker_client()
  proxy <- cl$container$run(
    "bobrik/socat",
    c("TCP4-LISTEN:2375,fork,reuseaddr", "UNIX-CONNECT:/var/run/docker.sock"),
    volumes = "/var/run/docker.sock:/var/run/docker.sock",
    ports = 2375,,
    rm = TRUE, detach = TRUE)
  on.exit(proxy$kill())

  ## The port on the proxy container:
  port <- proxy$ports()$host_port[[1]]

  ## Here we should try and wait:
  f <- function() {
    res <- curl::curl_fetch_memory(sprintf("http://localhost:%s/_ping", port))
    res$status_code == 200 && identical(rawToChar(res$content), "OK")
  }
  wait_until_ready(f)

  cl_http <- docker_client(host = sprintf("tcp://localhost:%s", port),
                           ignore_environment = TRUE)
  expect_equal(cl_http$ping(), cl$ping(), check.attributes = FALSE)
  expect_is(cl_http$container$list(), "data.frame")
})


test_that("connect over https", {
  skip_if_not_installed("withr")
  env <- test_machine_info()
  cl <- withr::with_envvar(env, docker_client())

  cfg <- cl$.api_client$http_client$config
  expect_equal(cfg$protocol, "https")

  expect_equivalent(cl$ping(), "OK")
  expect_is(cl$container$list(), "data.frame")
})


test_that("connect to machine", {
  env <- test_machine_info()
  cl <- docker_client(machine = env$DOCKER_MACHINE_NAME)

  cfg <- cl$.api_client$http_client$config
  expect_equal(cfg$protocol, "https")

  expect_equivalent(cl$ping(), "OK")
  expect_is(cl$container$list(), "data.frame")
})


test_that("debug http", {
  ## NOTE: this test, via libcurl, creates un-suppressable output
  cl <- test_docker_client() # ensures we can do docker things
  skip_on_cran()

  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "curl",
                          is_windows = FALSE,
                          debug = TRUE)
  cl <- http_client_curl(config)
  txt <- capture.output(res <- cl$ping())

  expect_match(
    txt,
    user_agent_header_string(config),
    fixed = TRUE, all = FALSE)
  expect_equal(res$content, charToRaw("OK"))

  ## to a file:
  dest <- tempfile()
  con <- file(dest, "wb")
  on.exit(close(con))
  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "curl",
                          is_windows = FALSE,
                          debug = con)
  res <- http_client_curl(config)$ping()
  close(con)
  on.exit()
  txt <- readLines(dest)

  expect_match(
    txt,
    user_agent_header_string(config),
    fixed = TRUE, all = FALSE)
  expect_equal(res$content, charToRaw("OK"))
})


test_that("debug http: binary", {
  cl <- test_docker_client() # ensures we can do docker things

  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "curl",
                          is_windows = FALSE,
                          debug = TRUE)
  cl <- http_client_curl(config)

  set.seed(1)
  dat <- sample(as.raw(255:0))
  txt <- capture.output(cl$request("POST", "/some/endpoint", body = dat))
  expect_match(txt, ">> <binary 256 bytes>", fixed = TRUE, all = FALSE)
})
