context("http (null)")


test_that("creation", {
  config <- docker_config(http_client_type = "null", ignore_environment = TRUE)

  cl <- http_client_null(config, NULL, NULL)
  expect_equal(cl$type, "null")
  expect_equal(cl$api_version, DOCKER_API_VERSION_DEFAULT)

  config <- docker_config(http_client_type = "null", ignore_environment = TRUE,
                          api_version = DOCKER_API_VERSION_MAX)
  cl <- http_client_null(config, NULL, NULL)
  expect_equal(cl$api_version, DOCKER_API_VERSION_MAX)
})


test_that("use", {
  config <- docker_config(http_client_type = "null", ignore_environment = TRUE)
  cl <- http_client_null(config, NULL, NULL)
  expect_error(cl$request(),
               "Can't make requests with the null client",
               fixed = TRUE)
})
