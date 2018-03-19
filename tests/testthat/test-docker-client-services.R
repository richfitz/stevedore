context("docker client: services")

test_that("create (offline)", {
  d <- null_docker_client()
  s <- d$services$get(dummy_id())
  expect_is(s, "docker_service")
  expect_equal(s$id(), dummy_id())
})


test_that("basic swarm service create", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  cnt <- cl$types$container_spec(image = "richfitz/iterate",
                                 command = c("1000", "1"))
  task <- cl$types$task_spec(container_spec = cnt)

  ans <- cl$services$create(name = "hello", task_template = task)

  expect_is(ans, "docker_service")
  expect_is(ans$id(), "character")
  expect_equal(ans$name(), "hello")
  expect_is(ans$version(), "integer")

  dat <- ans$inspect(FALSE)
  expect_equal(dat$id, ans$id())
  expect_equal(dat$version$index, ans$version(FALSE))

  dat <- cl$services$list()
  expect_true(ans$id() %in% dat$id)

  expect_null(ans$remove())
})