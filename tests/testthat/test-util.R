context("util")

test_that("camel <-> snake", {
  expect_equal(camel_to_snake("fooBar"), "foo_bar")
  expect_equal(camel_to_snake("foo_bar"), "foo_bar")
  expect_equal(snake_to_camel("fooBar"), "fooBar")
  expect_equal(snake_to_camel("foo_bar"), "fooBar")

  expect_equal(camel_to_snake(c("fooBar", "fizzBuzz")),
               c("foo_bar", "fizz_buzz"))
  expect_equal(snake_to_camel(c("foo_bar", "fizz_buzz")),
               c("fooBar", "fizzBuzz"))
})

test_that("case convert with consecutive capitals", {
  expect_equal(pascal_to_snake("NanoCPUs"), "nano_cpus")
  ## The conversion is lossy though:
  expect_equal(snake_to_pascal("nano_cpus"), "NanoCpus")
})

test_that("is_error", {
  expect_false(is_error(NULL))

  cond <- list(message = "foo", code = 404L, endpoint = "pull")
  class(cond) <- c("docker_error", "error", "condition")
  expect_true(is_error(cond))
})
