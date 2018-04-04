context("http (null)")

test_that("creation", {
  cl <- http_client_null(NULL, NULL, NULL, NULL)
  expect_equal(cl$type, "null")
  expect_equal(cl$api_version, DOCKER_API_VERSION_DEFAULT)

  cl <- http_client_null(NULL, "detect", NULL, NULL)
  expect_equal(cl$api_version, DOCKER_API_VERSION_DEFAULT)
})

test_that("use", {
  cl <- http_client_null(NULL, NULL, NULL, NULL)
  expect_error(cl$request(),
               "Can't make requests with the null client",
               fixed = TRUE)
})
