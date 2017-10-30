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
