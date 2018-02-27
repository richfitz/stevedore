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
  expect_error(assert_nonmissing(object), "'object' must be non-NA")

  expect_error(assert_nonmissing(NA_integer_), "must be non-NA")
  expect_error(assert_nonmissing(NA_real_), "must be non-NA")

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

test_that("assert_raw", {
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

test_that("assert_named", {
  object <- list(1, 2, 3)
  expect_error(assert_named(object), "'object' must be named")

  expect_error(assert_named(set_names(object, c("a", "b", "")), name = "x"),
               "All elements of 'x' must be named")
  expect_error(assert_named(set_names(object, c("a", "b", "b")), TRUE, "x"),
               "'x' must have unique names")
  expect_silent(assert_named(set_names(object, c("a", "b", "b")), FALSE, "x"))
  expect_silent(assert_named(set_names(object, c("a", "b", "c")), TRUE, "x"))
})

test_that("assert_directory", {
  path <- tempfile()
  expect_error(assert_directory(path), "'path' must be an existing directory")
  writeLines(character(0), path)
  expect_error(assert_directory(path), "'path' must be an existing directory")
  file.remove(path)
  dir.create(path)
  expect_silent(assert_directory(path))
  unlink(path)
})

test_that("assert_null", {
  object <- 1L
  expect_error(assert_null(object), "'object' must be NULL")
  expect_silent(assert_null(NULL))
})

test_that("assert_file_exists", {
  mypath <- tempfile()
  mypaths <- c(mypath, tempfile())
  expect_error(assert_file_exists(mypath),
               "File does not exist: ")
  expect_error(assert_file_exists(mypaths),
               "Files do not exist: ")

  writeLines("", mypath)
  writeLines("", mypaths[[2]])
  expect_silent(assert_file_exists(mypath))
  expect_silent(assert_file_exists(mypaths))
})

test_that("assert_function", {
  object <- NULL
  expect_error(assert_function(object), "'object' must be a function")
  expect_silent(assert_function(sin))
})

test_that("assert_empty_dots", {
  expect_silent(assert_empty_dots(name = "foo"))
  expect_error(assert_empty_dots(a = 1, name = "foo"),
               "Unknown argument passed to 'foo': a",
               fixed = TRUE)
  expect_error(assert_empty_dots(a = 1, b = 2, name = "foo"),
               "Unknown arguments passed to 'foo': a, b",
               fixed = TRUE)
  expect_error(assert_empty_dots(1, name = "foo"),
               "Unknown argument passed to 'foo': 1 positional argument",
               fixed = TRUE)
  expect_error(assert_empty_dots(1, 2, name = "foo"),
               "Unknown arguments passed to 'foo': 2 positional arguments",
               fixed = TRUE)
  expect_error(assert_empty_dots(a = 1, 1, name = "foo"),
               "Unknown arguments passed to 'foo': a, 1 positional argument",
               fixed = TRUE)
  expect_error(assert_empty_dots(a = 1, 1, 2, name = "foo"),
               "Unknown arguments passed to 'foo': a, 2 positional arguments",
               fixed = TRUE)
})


test_that("assert_nonempty_character", {
  value <- character()
  expect_error(assert_nonempty_character(value),
               "'value' must be a character vector (non zero length, non-NA)",
               fixed = TRUE)
  value <- c("a", NA)
  expect_error(assert_nonempty_character(value),
               "'value' must be character vector (non zero length, non-NA)",
               fixed = TRUE)
  value <- 1L
  expect_error(assert_nonempty_character(value),
               "'value' must be character vector (non zero length, non-NA)",
               fixed = TRUE)
  expect_silent(assert_nonempty_character("a"))
  expect_silent(assert_nonempty_character(letters))
})


test_that("assert_scalar_logical", {
  value <- character()
  expect_error(assert_scalar_logical(value),
               "'value' must be a scalar logical (non-NA)",
               fixed = TRUE)
  value <- c("a", NA)
  expect_error(assert_scalar_logical(value),
               "'value' must be a scalar logical (non-NA)",
               fixed = TRUE)
  value <- 1L
  expect_error(assert_scalar_logical(value),
               "'value' must be scalar logical (non-NA)",
               fixed = TRUE)
  expect_silent(assert_scalar_logical(TRUE))
  expect_silent(assert_scalar_logical(FALSE))
})


test_that("assert_scalar_character_or_null", {
  value <- 1
  expect_error(assert_scalar_character_or_null(value),
               "'value' must be a scalar character (non-NA), or NULL",
               fixed = TRUE)
  expect_silent(assert_scalar_character_or_null(NULL))
  expect_silent(assert_scalar_character_or_null("a"))
})

test_that("match_value", {
  object <- "foo"
  expect_error(match_value(object, letters), "'object' must be one of 'a', ")
  expect_silent(match_value("a", letters))
})
