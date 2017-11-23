context("util")

test_that("partial1: simple", {
  f <- function(a = 10, b = 100) {
    a + b
  }
  g <- partial1(f, 1)

  expect_equal(as.list(formals(g)), as.list(formals(f)[-1]))
  expect_equal(g(), 101)
  expect_equal(g(2), 3)
})

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

test_that("modify_args", {
  f <- function(a = 1, b = 2, c = 3, d = 4) {
    list(a = a, b = b, c = c, d = d)
  }
  g <- modify_args(f, "b", list(a = 10))
  expect_equal(g(), list(a = 10, b = 2, c = 3, d = 4))
  expect_equal(g(30, 40), list(a = 10, b = 2, c = 30, d = 40))
})
