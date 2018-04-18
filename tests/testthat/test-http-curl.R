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
