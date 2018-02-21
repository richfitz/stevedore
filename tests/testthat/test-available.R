context("docker_available")

test_that("invalid url", {
  expect_false(docker_available(NULL, "~", http_client_type = "null"))
  expect_silent(docker_available(NULL, "~", http_client_type = "null"))
  expect_message(
    docker_available(NULL, "~", http_client_type = "null", verbose = TRUE),
    "Failed to create docker client")
})

test_that("Nonexistent socket", {
  tmp <- tempfile()
  expect_false(docker_available(NULL, tmp, http_client_type = "null"))
  expect_silent(docker_available(NULL, tmp, http_client_type = "null"))
  expect_message(
    docker_available(NULL, tmp, http_client_type = "null", verbose = TRUE),
    "Failed to connect to docker daemon")
})
