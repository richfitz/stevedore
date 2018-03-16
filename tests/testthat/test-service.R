context("service")

test_that("create (offline)", {
  d <- null_docker_client()
  s <- d$services$get(dummy_id())
  expect_is(s, "docker_service")
  expect_equal(s$id(), dummy_id())
})
