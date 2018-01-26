context("http with httppipe")

test_that("basic connection works", {
  skip_if_no_httppipe_support()
  dc <- docker_client()
  dh <- docker_client(type = "httppipe")
  dh$ping()

  expect_identical(dc$version(), dh$version())
})

test_that("binary output", {
  skip_if_no_httppipe_support()
  dh <- docker_client(type = "httppipe")

  nm <- rand_str(10, "stevedore_")
  x <- dh$containers$create("bfirsh/reticulate-splines", name = nm)
  expect_identical(withVisible(x$start()),
                   list(value = x, visible = FALSE))
  logs <- x$logs()
  expect_is(logs, "docker_stream")
  expect_equal(logs[[1]], "Reticulating spline 1...\n")
  x$kill()
  x$remove()
})
