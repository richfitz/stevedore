context("http with httppipe")


test_that("construction", {
  skip_if_not_installed("httppipe")
  skip_if_not_using_docker()

  config <- docker_config(ignore_environment = TRUE,
                          http_client_type = "httppipe")
  x <- http_client_httppipe(config)

  expect_is(x, "list")
  expect_equal(x$type, "httppipe")
  expect_is(x$request, "function")
  expect_false(x$can_stream)
  expect_is(x$ping, "function")

  expected <- parse_headers(rawToChar(x$ping()$headers))[["Api-Version"]]
  expect_equal(x$api_version, expected)
})


test_that("basic connection works", {
  skip_if_no_httppipe_support()
  dc <- test_docker_client()
  dh <- test_docker_client(http_client_type = "httppipe")
  dh$ping()

  expect_identical(dc$version(), dh$version())
})


test_that("binary output", {
  skip_if_no_httppipe_support()
  skip("FIXME")
  dh <- test_docker_client(http_client_type = "httppipe")

  nm <- rand_str(10, "stevedore_")
  x <- dh$container$create("bfirsh/reticulate-splines", name = nm)
  expect_identical(withVisible(x$start()),
                   list(value = x, visible = FALSE))
  logs <- x$logs()
  expect_is(logs, "docker_stream")
  expect_equal(logs[[1]], "Reticulating spline 1...\n")
  x$kill()
  x$remove()
})


test_that("version detect", {
  invisible(test_docker_client()) # skips if not present
  skip_if_no_httppipe_support()

  config <- docker_config(api_version = NULL, http_client_type = "httppipe",
                          ignore_environment = TRUE)
  cl <- http_client(config, min_version = "0.0.1", max_version = "9.9.9")
  expect_equal(cl$api_version,
               raw_to_json(cl$request("GET", "/version")$content)$ApiVersion)
})
