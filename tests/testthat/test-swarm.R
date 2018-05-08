context("swarm")


test_that("swarm init/leave", {
  cl <- test_docker_client()
  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  expect_is(id, "character")
  err <- get_error(cl$swarm$leave())
  expect_is(err, "docker_error")
  expect_equal(err$code, 503L)

  expect_null(cl$swarm$leave(TRUE))
  on.exit()
})
