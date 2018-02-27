context("http with httppipe")

test_that("basic connection works", {
  skip_if_no_httppipe_support()
  dc <- test_docker_client()
  dh <- test_docker_client(http_client_type = "httppipe")
  dh$ping()

  expect_identical(dc$version(), dh$version())
})

test_that("binary output", {
  skip_if_no_httppipe_support()
  dh <- test_docker_client(http_client_type = "httppipe")

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

test_that("detect", {
  invisible(test_docker_client()) # skips if not present
  cl <- http_client(api_version = "detect",
                    type = "httppipe",
                    min_version = "0.0.1",
                    max_version = "9.9.9")
  expect_equal(cl$api_version,
               raw_to_json(cl$request("GET", "/version")$content)$ApiVersion)
})
