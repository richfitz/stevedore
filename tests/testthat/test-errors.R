context("errors")


test_that("is_docker_error_not_found", {
  expect_false(is_docker_error_not_found(NULL))
  expect_false(is_docker_error_not_found(
    structure(list(code = 403), class = "docker_error")))
  expect_true(is_docker_error_not_found(
    structure(list(code = 404), class = "docker_error")))
})


test_that("is_error", {
  expect_false(is_error(NULL))
  expect_false(is_error(
    structure(list(code = 403), class = "not_an_error")))
  expect_true(is_error(
    structure(list(code = 404), class = "condition")))
})


test_that("build_error", {
  e <- build_error("my message")
  expect_is(e, "condition")
  expect_is(e, "error")
  expect_is(e, "build_error")
  expect_equal(e$message, "my message")
})


test_that("push_error", {
  e <- push_error("my message")
  expect_is(e, "condition")
  expect_is(e, "error")
  expect_is(e, "push_error")
  expect_equal(e$message, "my message")
})


test_that("container_error", {
  container <- structure(TRUE, class = "docker_container")
  image <- structure(list(name = function() "image_name",
                          class = "docer_image"))

  out <- "my error message"

  e <- container_error(container, 1L, "cmd", image, out)
  expect_is(e, "condition")
  expect_is(e, "error")
  expect_is(e, "container_error")
  expect_equal(e$message, "Command 'cmd' in image 'image_name' returned non-zero exit status 1\nmy error message")

  e <- container_error(container, 1L, "cmd", image,
                       docker_stream(out, 1))
  expect_equal(e$message, "Command 'cmd' in image 'image_name' returned non-zero exit status 1")

  e <- container_error(container, 1L, "cmd", image,
                       docker_stream(c(out, "something else"), 2:1))
  expect_equal(e$message, "Command 'cmd' in image 'image_name' returned non-zero exit status 1\nmy error message")
})
