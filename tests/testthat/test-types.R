context("types")

test_that("handler: scalar atomic", {
  h <- swagger_type_make_handler_scalar_atomic("foo", "integer")
  expect_equal(h(1L), jsonlite::unbox(1L))
  expect_error(h(pi), "'foo' must be a scalar integer")
})


test_that("handler: vector atomic", {
  h <- swagger_type_make_handler_vector_atomic("foo", "integer")
  expect_equal(h(1L), 1L)
  expect_equal(h(integer()), integer())
  expect_equal(h(1:3), 1:3)
  expect_error(h(pi), "'foo' must be integer")
})


test_that("handler: string map", {
  h <- swagger_type_make_handler_string_map("foo")

  expect_null(h(NULL))
  expect_null(h(character()))

  expect_equal(h(c(key = "value")), list(key = jsonlite::unbox("value")))
  expect_equal(h(c(key1 = "value1", key2 = "value2")),
               list(key1 = jsonlite::unbox("value1"),
                    key2 = jsonlite::unbox("value2")))
  expect_equal(h(list(key1 = "value1", key2 = "value2")),
               list(key1 = jsonlite::unbox("value1"),
                    key2 = jsonlite::unbox("value2")))

  expect_error(h(list(key1 = letters)),
               "All elements of 'foo' must be scalar character")
})


test_that("handler: enum", {
  h <- swagger_type_make_handler_enum("foo", letters)
  expect_identical(h("a"), jsonlite::unbox("a"))
  expect_error(h("other"), "'foo' must be one of 'a', 'b',", fixed = TRUE)
})


test_that("handler: subtype", {
  h_foo <- swagger_type_make_handler_scalar_atomic("foo", "integer")
  types <- list(foo = list(handler = h_foo))
  h <- swagger_type_make_handler_subtype("foo", "bar", types)

  expect_identical(h(1L), jsonlite::unbox(1L))
  expect_error(h(2:3), "while processing 'bar':\n'foo' must be")
})


## Once this fails then the package is more complete and the
## conditional message can become an unconditional error:
test_that("unhandled types", {
  skip_if_not_installed("withr")
  expect_message(
    withr::with_options(list(stevedore.verbose.missing.types = TRUE),
                        swagger_types("1.29", swagger_spec_read("1.29"))),
    "Skipping [0-9]+/[0-9]+ handlers for")
})


test_that("empty types", {
  types <- docker_api_client_data("1.29")$types

  expect_null(types$health_config$reciever())
  expect_null(types$health_config$handler(NULL))
  expect_null(types$health_config$handler(list()))
})


test_that("pass through", {
  types <- docker_api_client_data("1.29")$types

  x <- structure(list(a = 1, b = 2),
                 class = "stevedore_type", typename = "health_config")

  expect_identical(types$health_config$handler(x), x)
  expect_error(
    types$dns_config$handler(x),
    "Unexpected input: recieved a 'health_config' when 'dns_config' was")
})


test_that("unexpected properties", {
  types <- docker_api_client_data("1.29")$types

  expect_error(types$health_config$handler(list(a = 1)),
               "Unexpected property 'a' in 'health_config'")
  expect_error(types$health_config$handler(list(a = 1, b = 2)),
               "Unexpected properties 'a', 'b' in 'health_config'")
})


test_that("basic handling", {
  types <- docker_api_client_data("1.29")$types

  dat <- types$health_config$reciever(test = "x")
  expect_equal(dat,
               stevedore_type(list(Test = "x"), "health_config"))
})


test_that("unhandled types", {
  ## This one will need to move around as we work with improving type
  ## coverage
  types <- docker_api_client_data("1.29")$types
  expect_error(types$mount$reciever(bind_options = "whatever"),
               "Handler for 'bind_options' (in 'mount') not yet implemented",
               fixed = TRUE)
})


test_that("nested types", {
  types <- docker_api_client_data("1.29")$types
  expect_null(types$container_spec$reciever(health_check = NULL))

  hc <- types$health_config$reciever(test = "foo")
  expect_equal(types$container_spec$reciever(health_check = hc),
               stevedore_type(list(HealthCheck = hc), "container_spec"))
  expect_equal(types$container_spec$reciever(health_check = list(test = "foo")),
               stevedore_type(list(HealthCheck = hc), "container_spec"))

  expect_error(
    types$container_spec$reciever(dns_config = hc),
    "Unexpected input: recieved a 'health_config' when 'dns_config' was")
  expect_error(
    types$container_spec$reciever(dns_config = list(test = "foo")),
    "while processing 'container_spec':\nUnexpected property")
})
