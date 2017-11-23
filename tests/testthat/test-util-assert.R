context("util (assert)")

test_that("assert_character", {
  object <- NULL
  expect_error(assert_character(object), "'object' must be a character")

  expect_error(assert_character(1), "must be a character")
  expect_error(assert_character(pi), "must be a character")

  expect_silent(assert_character("fred"))
})

test_that("assert_nonmissing", {
  object <- NA
  expect_error(assert_nonmissing(object), "'object' must not be NA")

  expect_error(assert_nonmissing(NA_integer_), "must not be NA")
  expect_error(assert_nonmissing(NA_real_), "must not be NA")

  expect_silent(assert_nonmissing(TRUE))
})

test_that("assert_scalar", {
  object <- 1:5
  expect_error(assert_scalar(object), "'object' must be a scalar")

  expect_error(assert_scalar(NULL), "must be a scalar")

  expect_silent(assert_scalar(TRUE))
})

test_that("assert_logical", {
  object <- NULL
  expect_error(assert_logical(object), "'object' must be logical")

  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical(pi), "must be logical")

  expect_silent(assert_logical(TRUE))
})

test_that("assert_is", {
  object <- NULL
  expect_error(assert_is(object, "data.frame"), "'object' must be a data.frame")

  expect_error(assert_is(1, "data.frame"), "must be a data.frame")
  expect_error(assert_is(pi, "data.frame"), "must be a data.frame")

  expect_silent(assert_is(mtcars, "data.frame"))
})

test_that("assert_logical", {
  object <- NULL
  expect_error(assert_raw(object), "'object' must be raw")

  expect_error(assert_raw(1), "must be raw")
  expect_error(assert_raw(pi), "must be raw")

  expect_silent(assert_raw(raw()))
})

test_that("assert_integer", {
  object <- NULL
  expect_error(assert_integer(object), "'object' must be integer")

  expect_error(assert_integer(1.12), "must be integer")
  expect_error(assert_integer(pi), "must be integer")

  expect_silent(assert_integer(1))
  expect_silent(assert_integer(1L))

  expect_error(assert_integer(1, strict = TRUE), "must be integer")
})

test_that("match_value", {
  object <- "foo"
  expect_error(match_value(object, letters), "'object' must be one of 'a', ")
  expect_silent(match_value("a", letters))
})
