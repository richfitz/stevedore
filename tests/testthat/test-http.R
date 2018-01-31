context("http")

test_that("version", {
  cl <- http_client()
  d <- test_docker_client()

  min <- MIN_DOCKER_API_VERSION
  max <- MAX_DOCKER_API_VERSION
  def <- DEFAULT_DOCKER_API_VERSION

  expect_identical(http_client_api_version(NULL, cl, min, max),
                   DEFAULT_DOCKER_API_VERSION)
  expect_identical(
    http_client_api_version(numeric_version("1.28"), cl, min, max),
    "1.28")
  expect_identical(http_client_api_version("1.28", cl, min, max), "1.28")


  expect_message(
    ans <- http_client_api_version("1.28", cl, "1.01", "1.04"),
    "Requested API version '1.28' is above max version '1.04'; downgrading",
    fixed = TRUE)
  expect_equal(ans, "1.04")
  expect_message(
    ans <- http_client_api_version("1.28", cl, "2.01", "2.04"),
    "Requested API version '1.28' is below min version '2.01'; upgrading",
    fixed = TRUE)
  expect_equal(ans, "2.01")

  f <- function() "1.23"
  expect_equal(http_client_api_version("detect", f, "1.0", "9.99"),
               "1.23")

  expect_message(
    ans <- http_client_api_version("detect", f, "1.01", "1.04"),
    "Detected API version '.+' is above max version '1.04'; downgrading")
  expect_equal(ans, "1.04")
  expect_message(
    ans <- http_client_api_version("detect", f, "2.01", "2.04"),
    "Detected API version '.+' is below min version '2.01'; upgrading")
  expect_equal(ans, "2.01")
})

test_that("version_response", {
  d <- test_docker_client()
  x <- d$containers$run("nginx", ports = TRUE, rm = TRUE,
                        detach = TRUE)
  on.exit(x$stop(t = 0))

  url <- sprintf("http://localhost:%s/404", x$ports()$host_port)
  res <- curl::curl_fetch_memory(url)

  expect_error(version_response(res),
               rawToChar(res$content),
               fixed = TRUE)
})
