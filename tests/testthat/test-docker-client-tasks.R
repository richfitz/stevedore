context("docker client: tasks")

test_that("create (offline)", {
  d <- null_docker_client()
  s <- d$task$get(dummy_id())
  expect_is(s, "docker_task")
  expect_equal(s$id(), dummy_id())
})


test_that("task logs", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  ans <- cl$service$create(name = "hello",
                           image = "richfitz/iterate",
                           args = c("1000", "1"),
                           timeout = 20,
                           time_wait_stable = 2,
                           stream = NULL)
  t <- ans$tasks()[[1]]
  expect_is(t, "docker_task")

  out <- t$logs(stream = FALSE)
  expect_is(out, "docker_stream")
  expect_match(out, "Doing 1000 iterations with interval 1", all = FALSE)

  ans$remove()
})
