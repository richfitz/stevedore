context("support")

test_that("as_docker_filter", {
  fil <- c(1:10)
  expect_error(as_docker_filter(fil), "'fil' must be named")
  fil <- c(a = 1, b = 2)
  expect_error(as_docker_filter(fil), "'fil' must be a character")

  expect_null(as_docker_filter(NULL))

  expect_equal(as.character(as_docker_filter(c(a = "foo"))),
               '{"a":["foo"]}')
  expect_equal(as.character(as_docker_filter(list(a = "foo"))),
               '{"a":["foo"]}')
  expect_equal(as.character(as_docker_filter(list(a = c("foo", "bar")))),
               '{"a":["foo","bar"]}')

  ## Pass json through untouched
  tmp <- jsonlite::toJSON(mtcars)
  expect_identical(as_docker_filter(tmp), tmp)
})
