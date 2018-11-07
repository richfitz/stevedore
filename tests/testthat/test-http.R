context("http")

test_that("version", {
  ping <- function() {
    list(headers = charToRaw("Api-Version: 1.23"),
         status_code = 200)
  }

  min <- DOCKER_API_VERSION_MIN
  max <- DOCKER_API_VERSION_MAX
  def <- DOCKER_API_VERSION_DEFAULT

  expect_identical(
    http_client_api_version(numeric_version("1.28"), ping, min, max),
    "1.28")
  expect_identical(http_client_api_version("1.28", ping, min, max), "1.28")

  expect_message(
    ans <- http_client_api_version("1.28", ping, "1.01", "1.04"),
    "Requested API version '1.28' is above max version '1.04'; downgrading",
    fixed = TRUE)
  expect_equal(ans, "1.04")
  expect_message(
    ans <- http_client_api_version("1.28", ping, "2.01", "2.04"),
    "Requested API version '1.28' is below min version '2.01'; upgrading",
    fixed = TRUE)
  expect_equal(ans, "2.01")

  expect_equal(http_client_api_version(NULL, ping, "1.0", "9.99"),
               "1.23")

  expect_message(
    ans <- http_client_api_version(NULL, ping, "1.01", "1.04"),
    "Detected API version '.+' is above max version '1.04'; downgrading")
  expect_equal(ans, "1.04")
  expect_message(
    ans <- http_client_api_version(NULL, ping, "2.01", "2.04"),
    "Detected API version '.+' is below min version '2.01'; upgrading")
  expect_equal(ans, "2.01")
})


test_that("ping_version", {
  res <- list(status_code = 200L, headers = charToRaw("Api-Version: 1.23"))
  expect_equal(ping_version(res), "1.23")

  res <- list(status_code = 404L,
              headers = charToRaw("Api-Version: 1.23"),
              content = charToRaw("nope"))
  expect_error(ping_version(res), "nope")

  res <- list(status_code = 200, headers = charToRaw("A: B"))
  expect_error(ping_version(res),
               "Failed to detect version.  Headers returned were: 'a'",
               fixed = TRUE)
})


test_that("ping_version: bad query", {
  ## Generate a bad query with:
  ## d <- test_docker_client()
  ## x <- d$container$run("nginx", ports = TRUE, rm = TRUE,
  ##                      detach = TRUE)
  ## on.exit(x$stop(t = 0))
  ## url <- sprintf("http://localhost:%s/404", x$ports()$host_port)
  ## res <- curl::curl_fetch_memory(url)
  res <- list(
    url = "http://localhost:32832/404",
    status_code = 404L,
    headers = as.raw(c(
      0x48, 0x54, 0x54, 0x50, 0x2f, 0x31, 0x2e,
      0x31, 0x20, 0x34, 0x30, 0x34, 0x20, 0x4e, 0x6f, 0x74, 0x20,
      0x46, 0x6f, 0x75, 0x6e, 0x64, 0x0d, 0x0a, 0x53, 0x65, 0x72,
      0x76, 0x65, 0x72, 0x3a, 0x20, 0x6e, 0x67, 0x69, 0x6e, 0x78,
      0x2f, 0x31, 0x2e, 0x31, 0x33, 0x2e, 0x38, 0x0d, 0x0a, 0x44,
      0x61, 0x74, 0x65, 0x3a, 0x20, 0x46, 0x72, 0x69, 0x2c, 0x20,
      0x32, 0x33, 0x20, 0x4d, 0x61, 0x72, 0x20, 0x32, 0x30, 0x31,
      0x38, 0x20, 0x31, 0x35, 0x3a, 0x32, 0x32, 0x3a, 0x32, 0x33,
      0x20, 0x47, 0x4d, 0x54, 0x0d, 0x0a, 0x43, 0x6f, 0x6e, 0x74,
      0x65, 0x6e, 0x74, 0x2d, 0x54, 0x79, 0x70, 0x65, 0x3a, 0x20,
      0x74, 0x65, 0x78, 0x74, 0x2f, 0x68, 0x74, 0x6d, 0x6c, 0x0d,
      0x0a, 0x43, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d, 0x4c,
      0x65, 0x6e, 0x67, 0x74, 0x68, 0x3a, 0x20, 0x31, 0x36, 0x39,
      0x0d, 0x0a, 0x43, 0x6f, 0x6e, 0x6e, 0x65, 0x63, 0x74, 0x69,
      0x6f, 0x6e, 0x3a, 0x20, 0x6b, 0x65, 0x65, 0x70, 0x2d, 0x61,
      0x6c, 0x69, 0x76, 0x65, 0x0d, 0x0a, 0x0d, 0x0a)),
    modified = structure(NA_integer_, class = c("POSIXct", "POSIXt")),
    times = c(redirect = 0, namelookup = 0.00156, connect = 0.00173,
              pretransfer = 0.001768, starttransfer = 0.003169,
              total = 0.003185),
    content = as.raw(c(
      0x3c,
      0x68, 0x74, 0x6d, 0x6c, 0x3e, 0x0d, 0x0a, 0x3c, 0x68, 0x65,
      0x61, 0x64, 0x3e, 0x3c, 0x74, 0x69, 0x74, 0x6c, 0x65, 0x3e,
      0x34, 0x30, 0x34, 0x20, 0x4e, 0x6f, 0x74, 0x20, 0x46, 0x6f,
      0x75, 0x6e, 0x64, 0x3c, 0x2f, 0x74, 0x69, 0x74, 0x6c, 0x65,
      0x3e, 0x3c, 0x2f, 0x68, 0x65, 0x61, 0x64, 0x3e, 0x0d, 0x0a,
      0x3c, 0x62, 0x6f, 0x64, 0x79, 0x20, 0x62, 0x67, 0x63, 0x6f,
      0x6c, 0x6f, 0x72, 0x3d, 0x22, 0x77, 0x68, 0x69, 0x74, 0x65,
      0x22, 0x3e, 0x0d, 0x0a, 0x3c, 0x63, 0x65, 0x6e, 0x74, 0x65,
      0x72, 0x3e, 0x3c, 0x68, 0x31, 0x3e, 0x34, 0x30, 0x34, 0x20,
      0x4e, 0x6f, 0x74, 0x20, 0x46, 0x6f, 0x75, 0x6e, 0x64, 0x3c,
      0x2f, 0x68, 0x31, 0x3e, 0x3c, 0x2f, 0x63, 0x65, 0x6e, 0x74,
      0x65, 0x72, 0x3e, 0x0d, 0x0a, 0x3c, 0x68, 0x72, 0x3e, 0x3c,
      0x63, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x3e, 0x6e, 0x67, 0x69,
      0x6e, 0x78, 0x2f, 0x31, 0x2e, 0x31, 0x33, 0x2e, 0x38, 0x3c,
      0x2f, 0x63, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x3e, 0x0d, 0x0a,
      0x3c, 0x2f, 0x62, 0x6f, 0x64, 0x79, 0x3e, 0x0d, 0x0a, 0x3c,
      0x2f, 0x68, 0x74, 0x6d, 0x6c, 0x3e, 0x0d, 0x0a)))

    expect_error(ping_version(res),
                 rawToChar(res$content),
                 fixed = TRUE)
})


test_that("response_to_error", {
  make_response <- function(status_code, headers, content) {
    headers <- paste(sprintf("%s: %s\n", names(headers), unname(headers)),
                     collapse = "")
    if (!is.raw(content)) {
      content <- charToRaw(as.character(jsonlite::toJSON(content)))
    }
    list(status_code = status_code,
         headers = charToRaw(headers),
         content = content)
  }

  ## The simplest case:
  r <- make_response(404, c("Content-Type" = "application/json"),
                     list(message = jsonlite::unbox("my error")))
  e <- get_error(response_to_error(r, "myendpoint", "myreason"))
  expect_is(e, "error")
  expect_is(e, "condition")
  expect_is(e, "docker_error")

  expect_equal(e$message, "my error")
  expect_equal(e$code, 404)
  expect_equal(e$endpoint, "myendpoint")
  expect_equal(e$reason, "myreason")

  r$content <- raw()
  expect_error(response_to_error(r, "myendpoint", "myreason"),
               "An error occured but HEAD is obscuring it")

  r <- make_response(404, c("Content-Type" = "text/plain"),
                     charToRaw("plain text error"))
  e <- get_error(response_to_error(r, "myendpoint", "myreason"))
  expect_equal(e$message, "plain text error")
})


test_that("build_url_query: empty", {
  expect_null(build_url_query(NULL))
  expect_null(build_url_query(character()))
})


test_that("build_url_query: logical", {
  expect_equal(build_url_query(list(a = TRUE)),
               "?a=true")
  expect_equal(build_url_query(list(a = TRUE, b = FALSE)),
               "?a=true&b=false")
})


test_that("build_url_query: character", {
  expect_equal(build_url_query(list(a = "foo")),
               "?a=foo")
  expect_equal(build_url_query(list(a = "foo bar")),
               "?a=foo%20bar")
  expect_equal(build_url_query(list(a = "foo", b = FALSE)),
               "?a=foo&b=false")
})


test_that("build_url_query: repeat", {
  expect_equal(build_url_query(list(a = "foo", a = "bar")),
               "?a=foo&a=bar")
  expect_equal(build_url_query(list(a = c("foo", "bar"))),
               "?a=foo&a=bar")
})


test_that("build_url", {
  expect_equal(build_url("http://localhost", "1.29", "/dest"),
               "http://localhost/v1.29/dest")
  expect_equal(build_url("http://localhost", "1.29", "/dest", list(a = TRUE)),
               "http://localhost/v1.29/dest?a=true")
})


test_that("prepare_body", {
  expect_equal(prepare_body(as.raw(0:9)),
               list(raw = as.raw(0:9),
                    content_type = "application/octet-stream"))
  expect_equal(prepare_body("hello"),
               list(raw = charToRaw("hello"),
                    content_type = "application/json"))
})


## Should only be generatable by a server error I think, and then all
## error codes (>=300) are dealt with anyway.
test_that("unexpected status code", {
  expect_error(
    run_endpoint_handler(list(status_code = 201), list(), FALSE, NULL),
    "unexpected response code 201")
})
