context("docker client: tasks")

test_that("create (offline)", {
  d <- null_docker_client()
  s <- d$tasks$get(dummy_id())
  expect_is(s, "docker_task")
  expect_equal(s$id(), dummy_id())
})
