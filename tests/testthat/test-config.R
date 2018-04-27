context("config")

test_that("default unix config", {
  cfg <- docker_config_validate(api_version = NULL, host = NULL,
                                cert_path = NULL, tls_verify = NULL,
                                http_client_type = NULL,
                                as_is_names = FALSE, data_frame = identity,
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
                                as_is_names = FALSE, data_frame = identity,
                                is_windows = TRUE, quiet = FALSE)
  cmp <- docker_config_validate(api_version = NULL, host = NULL,
                                cert_path = NULL, tls_verify = NULL,
                                http_client_type = NULL,
                                as_is_names = FALSE, data_frame = identity,
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
  tls_path <- fake_tls_dir()
  on.exit(unlink(tls_path, recursive = TRUE), add = TRUE)

  cfg <- docker_config_validate(api_version = NULL,
                                host = "https://1.2.3.4:5678",
                                cert_path = tls_path,
                                tls_verify = TRUE,
                                http_client_type = NULL,
                                as_is_names = FALSE, data_frame = identity,
                                is_windows = FALSE, quiet = FALSE)

  tls_files <- c("key.pem", "ca.pem", "cert.pem")
  expect_null(cfg$api_version)
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


## Still not totally clear how to deal with overrides here!
test_that("from environment", {
  skip_if_not_installed("withr")

  tls_path <- fake_tls_dir()
  on.exit(unlink(tls_path, recursive = TRUE), add = TRUE)
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


test_that("from machine", {
  info <- test_machine_info()

  cfg <- docker_config(machine = info$DOCKER_MACHINE_NAME)
  expect_equal(cfg$addr, sub("^.+://", "", info$DOCKER_HOST))
  expect_equal(cfg$tls_verify, !is.null(info$DOCKER_TLS_VERIFY))
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
  on.exit(unlink(tls_path, recursive = TRUE), add = TRUE)
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


test_that("machine parse", {
  output <- c(
    'export DOCKER_TLS_VERIFY="1"',
    'export DOCKER_HOST="tcp://192.168.99.100:2376"',
    'export DOCKER_CERT_PATH="/Users/rich/.docker/machine/machines/extra"',
    'export DOCKER_MACHINE_NAME="extra"',
    '# Run this command to configure your shell: ',
    '# eval $(/usr/local/bin/docker-machine env --shell bash extra)')
  expect_equal(
    machine_env_parse(output),
    list(DOCKER_TLS_VERIFY = "1",
         DOCKER_HOST = "tcp://192.168.99.100:2376",
         DOCKER_CERT_PATH = "/Users/rich/.docker/machine/machines/extra",
         DOCKER_MACHINE_NAME = "extra"))
})


test_that("fake machine output", {
  skip_on_os("windows")
  skip_on_cran()

  tls <- normalizePath("tls")

  env <- c(DOCKER_TLS_VERIFY = "1",
           DOCKER_HOST = "tcp://192.168.99.100:9999",
           DOCKER_CERT_PATH = tls,
           DOCKER_MACHINE_NAME = "extra")
  p <- fake_docker_machine(env)
  on.exit(unlink(p, recursive = TRUE))
  dat <- withr::with_path(p, get_machine_env("extra"))
  expect_equal(
    dat,
    list(DOCKER_TLS_VERIFY = "1",
         DOCKER_HOST = "tcp://192.168.99.100:9999",
         DOCKER_CERT_PATH = tls,
         DOCKER_MACHINE_NAME = "extra"))

  dat <- withr::with_path(p, docker_config(machine = "extra"))

  expect_true(dat$tls_verify)
  expect_equal(dirname(dat$cert$key), tls)
  expect_equal(dat$base_url, "https://192.168.99.100:9999")
  expect_equal(dat$protocol, "https")
})
