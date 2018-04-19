context("default client")

test_that("set default", {
  default_client_del()
  default_client_set(http_client_type = "null")
  expect_equal(default_client_get()$connection_info()$http_client_type,
               "null")
  expect_equal(docker$connection_info()$http_client_type,
               "null")
})


test_that("set default with client", {
  default_client_del()
  cl <- docker_client(http_client_type = "null", host = "tcp://1.2.4.5:5678")
  prev <- default_client_set(cl)
  expect_identical(default_client_get(), cl)
  expect_identical(docker, cl)
  expect_null(prev)
  ans <- default_client_set(NULL)
  expect_identical(ans, cl)
  expect_null(.stevedore$default_client)
})


test_that("set default with client", {
  default_client_del()
  cl <- docker_client(http_client_type = "null", host = "tcp://1.2.4.5:5678")
  default_client_set(cl)
  expect_identical(default_client_get(), cl)
  expect_equal(docker, cl)
})


test_that("invalid usage", {
  default_client_del()
  msg <- "If setting a default client directly it must be the only argument"
  cl <- null_docker_client()
  expect_error(default_client_set(client = cl), msg, fixed = TRUE)
  expect_error(default_client_set(cl, 1), msg, fixed = TRUE)
})


test_that("clear defaults", {
  default_client_del()
  expect_null(.stevedore$default_client)
})


test_that("binding", {
  e <- new_empty_env()
  default_client_binding(e)
  expect_equal(names(e), "docker")
  expect_true(bindingIsActive("docker", e))
  lock_environment(e)
  expect_silent(default_client_binding(e))
  expect_is(e$docker, "docker_client")
})
