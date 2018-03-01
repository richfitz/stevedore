context("http")

test_that("version", {
  ping <- function() {
    list(headers = charToRaw("Api-Version: 1.23"),
         status_code = 200)
  }

  min <- MIN_DOCKER_API_VERSION
  max <- MAX_DOCKER_API_VERSION
  def <- DEFAULT_DOCKER_API_VERSION

  expect_identical(http_client_api_version(NULL, ping, min, max),
                   DEFAULT_DOCKER_API_VERSION)
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

  expect_equal(http_client_api_version("detect", ping, "1.0", "9.99"),
               "1.23")

  expect_message(
    ans <- http_client_api_version("detect", ping, "1.01", "1.04"),
    "Detected API version '.+' is above max version '1.04'; downgrading")
  expect_equal(ans, "1.04")
  expect_message(
    ans <- http_client_api_version("detect", ping, "2.01", "2.04"),
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
  d <- test_docker_client()
  x <- d$containers$run("nginx", ports = TRUE, rm = TRUE,
                        detach = TRUE)
  on.exit(x$stop(t = 0))

  url <- sprintf("http://localhost:%s/404", x$ports()$host_port)
  res <- curl::curl_fetch_memory(url)

  expect_error(ping_version(res),
               rawToChar(res$content),
               fixed = TRUE)
})


test_that("detect", {
  ## This requires the server
  invisible(test_docker_client())
  cl <- http_client(api_version = "detect",
                    min_version = "0.0.1",
                    max_version = "9.9.9")
  expect_equal(cl$api_version,
               raw_to_json(cl$request("GET", "/version")$content)$ApiVersion)
})


## There's more to test here - should also test if we can build the
## type of connection requested with the requested platform/url
test_that("client data", {
  ## default cases:
  expect_equal(http_client_data(NULL, NULL, TRUE),
               list(client_type = "httppipe",
                    url_type = "npipe",
                    base_url = DEFAULT_DOCKER_WINDOWS_PIPE))
  expect_equal(http_client_data(NULL, NULL, FALSE),
               list(client_type = "curl",
                    url_type = "socket",
                    base_url = DEFAULT_DOCKER_UNIX_SOCKET))

  ## Force httppipe connections:
  expect_equal(http_client_data(NULL, "httppipe", TRUE),
               list(client_type = "httppipe",
                    url_type = "npipe",
                    base_url = DEFAULT_DOCKER_WINDOWS_PIPE))
  expect_equal(http_client_data(NULL, "httppipe", FALSE),
               list(client_type = "httppipe",
                    url_type = "socket",
                    base_url = DEFAULT_DOCKER_UNIX_SOCKET))

  ## Force curl connections:
  expect_error(http_client_data(NULL, "curl", TRUE),
               "The 'curl' http driver cannot connect to named pipes")
  expect_equal(http_client_data(NULL, "curl", FALSE),
               list(client_type = "curl",
                    url_type = "socket",
                    base_url = DEFAULT_DOCKER_UNIX_SOCKET))

  ## Force named pipes
  pipe <- "npipe:////./pipe/mypipe"
  expect_equal(http_client_data(pipe, NULL, TRUE),
               list(client_type = "httppipe",
                    url_type = "npipe",
                    base_url = pipe))
  expect_error(http_client_data(pipe, NULL, FALSE),
               "Named pipe connections are only available on windows")

  ## Force sockets
  tmp <- tempfile()
  expect_error(http_client_data(tmp, NULL, TRUE),
               "Socket connections are not available on windows")
  expect_equal(http_client_data(tmp, NULL, FALSE),
               list(client_type = "curl",
                    url_type = "socket",
                    base_url = tmp))

  ## Force http
  http <- "http://host/path"
  expect_equal(http_client_data(http, NULL, TRUE),
               list(client_type = "curl",
                    url_type = "http",
                    base_url = http))
  expect_equal(http_client_data(http, NULL, FALSE),
               list(client_type = "curl",
                    url_type = "http",
                    base_url = http))
  expect_error(http_client_data(http, "httppipe", FALSE),
               "The 'httppipe' http driver cannot connect to http servers")
  expect_error(http_client_data(http, "httppipe", TRUE),
               "The 'httppipe' http driver cannot connect to http servers")
})

test_that("url type", {
  expect_equal(http_url_type(DEFAULT_DOCKER_UNIX_SOCKET), "socket")
  expect_equal(http_url_type(DEFAULT_DOCKER_WINDOWS_PIPE), "npipe")
  expect_equal(http_url_type("http://host/path"), "http")
  expect_equal(http_url_type("https://host/path"), "http")
  expect_error(http_url_type("ftp://noway"),
               "Can't detect url type from 'ftp://noway'")
})

test_that("can't specify http url yet", {
  expect_error(test_docker_client(url = "http://localhost:1234"),
               "Providing docker http/https url is not currently supported",
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
})


test_that("build_url", {
  expect_equal(build_url("http://localhost", "1.29", "/dest"),
               "http://localhost/v1.29/dest")
  expect_equal(build_url("http://localhost", "1.29", "/dest", list(a = TRUE)),
               "http://localhost/v1.29/dest?a=true")
})
