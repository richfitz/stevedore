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
                                 args = c("1000", "1"))
  task <- cl$types$task_spec(container_spec = cnt)

  ans <- cl$services$create(name = "hello", task_template = task,
                            timeout = 20, time_wait_stable = 0,
                            stream = NULL)

  expect_is(ans, "docker_service")
  expect_is(ans$id(), "character")
  expect_equal(ans$name(), "hello")
  expect_is(ans$version(), "integer")

  dat <- ans$inspect(FALSE)
  expect_equal(dat$id, ans$id())
  expect_equal(dat$version$index, ans$version(FALSE))
  expect_equal(dat$spec$name, "hello")
  expect_equal(dat$spec$task_template$container_spec$image, "richfitz/iterate")
  expect_equal(dat$spec$task_template$container_spec$args, c("1000", "1"))

  dat <- cl$services$list()
  expect_true(ans$id() %in% dat$id)

  expect_null(ans$remove())
})


test_that("basic swarm service create - expanded types", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  ans <- cl$services$create(name = "hello",
                            image = "richfitz/iterate",
                            args = c("1000", "1"),
                            timeout = 20,
                            time_wait_stable = 0,
                            stream = NULL)

  expect_is(ans, "docker_service")
  expect_is(ans$id(), "character")
  expect_equal(ans$name(), "hello")
  expect_is(ans$version(), "integer")

  dat <- ans$inspect(FALSE)
  expect_equal(dat$id, ans$id())
  expect_equal(dat$version$index, ans$version(FALSE))
  expect_equal(dat$spec$name, "hello")
  expect_equal(dat$spec$task_template$container_spec$image, "richfitz/iterate")
  expect_equal(dat$spec$task_template$container_spec$args, c("1000", "1"))

  dat <- cl$services$list()
  expect_true(ans$id() %in% dat$id)

  t <- cl$tasks$get(cl$tasks$list(list('desired-state' = "running"))$id)
  expect_equal(t$state(), "running")
  s <- t$service()
  expect_equal(s$id(), ans$id())
  expect_is(s, "docker_service")

  ts <- ans$tasks()
  expect_is(ts, "list")
  expect_is(ts[[1]], "docker_task")
  expect_equal(ts[[1]]$id(), t$id())

  expect_equal(ans$tasks(list("desired-state" = "shutdown")), list())
  expect_error(ans$tasks(list(service = "foo")),
               "'service' is not a valid filter name for this method")

  expect_null(ans$remove())
})


test_that("replicas & swarm ps", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  n <- 3L
  ans <- cl$services$create(name = "redis",
                            image = "redis",
                            replicas = n,
                            timeout = 20,
                            time_wait_stable = 0,
                            stream = NULL)

  ps <- ans$ps()
  expect_is(ps, "data.frame")
  expect_equal(ps$name, sprintf("redis.%d", seq_len(n)))
  expect_equal(ps$image, rep("redis", n))
  expect_equal(ps$desired_state, rep("running", n))
  expect_equal(ps$current_state, rep("running", n))
  expect_match(ps$when, "ago$")
  expect_is(ps$id, "character")
  expect_is(ps$node, "character")

  ps2 <- ans$ps(FALSE)
  v <- setdiff(names(ps), c("name", "node", "when"))
  expect_equal(ps[v], ps2[v])
  expect_match(ps2$when, "ago$")
  expect_match(ps2$name, ans$id())
  expect_match(ps2$node, cl$nodes$list()[[1L]])

  ans$remove()
})


test_that("convergence: failure", {
  ## A failing service:
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  expect_error(
    cl$services$create(name = "myservice",
                       image = "richfitz/iterate",
                       command = "error-command",
                       replicas = 2,
                       timeout = 2,
                       stream = NULL),
    "service has not converged in time")
  s <- cl$services$get("myservice")
  expect_is(s, "docker_service")
  s$remove()
})


test_that("don't detach", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  s <- cl$services$create(name = "myservice",
                          image = "richfitz/iterate",
                          command = "error-command",
                          detach = TRUE)
  expect_is(s, "docker_service")
  s$remove()
})
