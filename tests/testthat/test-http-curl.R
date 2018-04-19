context("http with curl")

test_that("construction", {
  skip_on_windows()

  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "curl",
                          is_windows = FALSE)
  cl <- http_client_curl(config)

  expect_is(cl, "list")
  expect_equal(cl$type, "curl")
  expect_is(cl$request, "function")
  expect_equal(cl$api_version, DOCKER_API_VERSION_DEFAULT)
  expect_true(cl$can_stream)
  expect_is(cl$ping, "function")
})


test_that("version detect", {
  ## This requires the server
  invisible(test_docker_client())
  config <- docker_config(api_version = "detect", http_client_type = "curl",
                          ignore_environment = TRUE)
  cl <- http_client(config, min_version = "0.0.1", max_version = "9.9.9")
  expect_equal(cl$api_version,
               raw_to_json(cl$request("GET", "/version")$content)$ApiVersion)
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

  expect_error(openssl::read_p12(path, "anotherpass"))
  expect_silent(dat <- openssl::read_p12(path, "mypass"))

  path2 <- write_p12("tls/key.pem", "tls/ca.pem", "tls/cert.pem",
                     "stevedore-test-p12", "mypass", FALSE)
  expect_error(openssl::read_p12(path2, "anotherpass"))
  expect_silent(dat2 <- openssl::read_p12(path2, "mypass"))
  expect_equal(dat, dat2)
  expect_false(identical(read_binary(path), read_binary(path2)))
})


test_that("options: https + secure transport", {
  .stevedore$curl_uses_secure_transport <- TRUE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())

  tls_path <- "tls"
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
})


test_that("options: http", {
  .stevedore$curl_uses_secure_transport <- FALSE
  on.exit(.stevedore$curl_uses_secure_transport <- curl_uses_secure_transport())

  cfg <- docker_config(ignore_environment = TRUE,
                       host = "http://1.2.3.4:5678")
  expect_null(curl_handle_opts(cfg))
})


test_that("make handle", {
  cfg <- docker_config(ignore_environment = TRUE,
                       host = "https://1.2.3.4:5678",
                       cert_path = "tls",
                       tls_verify = TRUE)
  opts <- curl_handle_opts(cfg)
  factory <- make_curl_handle(cfg)
  expect_is(factory, "function")

  expect_silent(h <- factory())
  expect_is(h, "curl_handle")
  ## no public function for getting current options
})
