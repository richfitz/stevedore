context("http with httppipe")

test_that("base_url with https not supported", {
  expect_error(http_client_curl("http://localhost:8888"),
               "Providing docker http/https url is not currently supported",
               fixed = TRUE)
})


test_that("construction", {
  skip_on_windows()

  cl <- http_client_curl(DEFAULT_DOCKER_UNIX_SOCKET)

  expect_is(cl, "list")
  expect_equal(cl$type, "curl")
  expect_is(cl$request, "function")
  expect_equal(cl$api_version, DOCKER_API_VERSION_DEFAULT)
  expect_true(cl$can_stream)
  expect_is(cl$ping, "function")
})
