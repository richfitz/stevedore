context("spec utilites")


test_that("startup", {
  stevedore:::.onLoad()
  expect_equal(.stevedore$client_data, list())
})


test_that("read missing spec is an error", {
  expect_error(swagger_spec_read("0.0.1"),
               "Invalid version 0.0.1; try one of", fixed = TRUE)
})
