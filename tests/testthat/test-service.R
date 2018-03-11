context("service")

test_that("create (offline)", {
  d <- docker_client(http_client_type = "null")
  s <- d$services$get(dummy_id())
  expect_is(s, "docker_service")
  expect_equal(s$id(), dummy_id())
})
