context("config")

test_that("default unix config", {
  cfg <- docker_config_validate(api_version = NULL, host = NULL,
                                cert_path = NULL, tls_verify = NULL,
                                http_client_type = NULL,
                                is_windows = FALSE, quiet = FALSE)
  expect_null(cfg$api_version)
  expect_equal(cfg$addr, sub("unix://", "", DEFAULT_DOCKER_UNIX_SOCKET))
  expect_equal(cfg$protocol, "socket")
  expect_false(cfg$use_tls)
  expect_false(cfg$tls_verify)
  expect_equal(cfg$http_client_type, "curl")
  expect_null(cfg$cert)
  expect_equal(cfg$base_url, "http://localhost")
})


test_that("default windows config", {
  cfg <- docker_config_validate(api_version = NULL, host = NULL,
                                cert_path = NULL, tls_verify = NULL,
                                http_client_type = NULL,
                                is_windows = TRUE, quiet = FALSE)
  cmp <- docker_config_validate(api_version = NULL, host = NULL,
                                cert_path = NULL, tls_verify = NULL,
                                http_client_type = NULL,
                                is_windows = FALSE, quiet = FALSE)
  expect_null(cfg$api_version)
  expect_equal(cfg$addr, DEFAULT_DOCKER_WINDOWS_PIPE)
  expect_equal(cfg$protocol, "npipe")
  expect_false(cfg$use_tls)
  expect_false(cfg$tls_verify)
  expect_equal(cfg$http_client_type, "httppipe")
  expect_null(cfg$cert)
  expect_equal(cfg$base_url, "http://localhost")
})


test_that("machine-compatible tcp", {
  path <- fake_tls_dir()

  cfg <- docker_config_validate(api_version = NULL,
                                host = "https://1.2.3.4:5678",
                                cert_path = path,
                                tls_verify = TRUE,
                                http_client_type = NULL,
                                is_windows = FALSE, quiet = FALSE)

  tls_files <- c("key.pem", "ca.pem", "cert.pem")
  expect_null(cfg$api_version)
  expect_equal(cfg$addr, "1.2.3.4:5678")
  expect_equal(cfg$protocol, "https")
  expect_true(cfg$use_tls)
  expect_true(cfg$tls_verify)
  expect_equal(cfg$http_client_type, "curl")
  expect_equal(cfg$cert,
               set_names(as.list(file.path(path, tls_files)),
                         sub(".pem", "", tls_files, fixed = TRUE)))
  expect_equal(cfg$base_url, "https://1.2.3.4:5678")
})


## Still not totally clear how to deal with overrides here!
test_that("from environment", {
  skip_if_not_installed("withr")

  tls_path <- fake_tls_dir()
  cfg <- withr::with_envvar(list(DOCKER_API_VERSION = "1.36",
                                 DOCKER_HOST = "https://1.2.3.4:5678",
                                 DOCKER_CERT_PATH = tls_path,
                                 DOCKER_TLS_VERIFY = "1"),
                            docker_config())

  tls_files <- c("key.pem", "ca.pem", "cert.pem")
  expect_equal(cfg$api_version, "1.36")
  expect_equal(cfg$addr, "1.2.3.4:5678")
  expect_equal(cfg$protocol, "https")
  expect_true(cfg$use_tls)
  expect_true(cfg$tls_verify)
  expect_equal(cfg$http_client_type, "curl")
  expect_equal(cfg$cert,
               set_names(as.list(file.path(tls_path, tls_files)),
                         sub(".pem", "", tls_files, fixed = TRUE)))
  expect_equal(cfg$base_url, "https://1.2.3.4:5678")
})


test_that("error cases", {
  expect_error(
    docker_config(is_windows = TRUE, http_client_type = "curl",
                  ignore_environment = TRUE),
    "The 'curl' http driver cannot connect to named pipes")
  expect_error(
    docker_config(is_windows = FALSE, host = DEFAULT_DOCKER_WINDOWS_PIPE,
                  ignore_environment = TRUE),
    "Named pipe connections are only available on windows")
  expect_error(
    docker_config(is_windows = TRUE, host = DEFAULT_DOCKER_UNIX_SOCKET,
                  ignore_environment = TRUE),
    "Socket connections are not available on windows")
  expect_error(
    docker_config(host = "http://1.2.3.4:5678", http_client_type = "httppipe",
                  ignore_environment = TRUE),
    "The 'httppipe' http driver cannot connect to http servers")
  expect_error(
    docker_config(host = "ftp://noway", ignore_environment = TRUE),
    "Unknown protocol 'ftp' for host 'ftp://noway'")
  expect_error(
    docker_config(host = "noway", ignore_environment = TRUE),
    "Invalid address 'noway' - must match '<protocol>://<addr>'",
    fixed = TRUE)

  tls_path <- fake_tls_dir()
  file.remove(file.path(tls_path, "key.pem"))
  expect_error(
    docker_config(is_windows = FALSE, ignore_environment = TRUE,
                  host = "https://1.2.3.4:5678",
                  cert_path = tls_path),
    "Certificate file missing within directory '.+': 'key.pem'")
  file.remove(file.path(tls_path, "ca.pem"))
  expect_error(
    docker_config(is_windows = FALSE, ignore_environment = TRUE,
                  host = "https://1.2.3.4:5678",
                  cert_path = tls_path),
    "Certificate files missing within directory '.+': 'key.pem', 'ca.pem'")
  expect_error(
    docker_config(is_windows = FALSE, ignore_environment = TRUE,
                  host = "https://1.2.3.4:5678"),
    "cert_path not given, but tls_verify requested")
  expect_error(
    docker_config(is_windows = FALSE, ignore_environment = TRUE,
                  host = "tcp://1.2.3.4:5678", tls_verify = TRUE),
    "cert_path not given, but tls_verify requested")
})
