context("configs")

test_that("create and delete", {
  cl <- docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  for (v in test_docker_versions()) {
    cl_v <- test_docker_client(api_version = v)

    key <- rand_str()

    if (numeric_version(v) < numeric_version("1.30")) {
      expect_error(
        cl_v$configs$create(key, "config"),
        "'config_create' (POST /configs/create) requires docker API version",
        fixed = TRUE)
      next
    }

    id <- cl_v$configs$create(key, "config!")
    expect_is(id, "character")
    expect_true(id %in% cl_v$configs$list()$id)
    expect_true(key %in% cl_v$configs$list()$name)

    dat <- cl_v$configs$inspect(id)
    expect_identical(cl_v$configs$inspect(key), dat)

    expect_null(cl_v$configs$remove(key))
    expect_false(id %in% cl_v$configs$list()$id)
    expect_false(key %in% cl_v$configs$list()$name)
  }
})


test_that("add to container", {
  cl <- test_docker_client(api_version = "1.30")

  cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  name <- "myconfig"
  data <- "config!"
  id <- cl$configs$create(name, data)

  ans <- cl$services$create(name = "redis",
                            image = "redis",
                            configs = name,
                            timeout = 20,
                            time_wait_stable = 0,
                            stream = NULL)

  tasks <- ans$tasks(list("desired-state" = "running"))
  expect_equal(length(tasks), 1L)

  container_id <- tasks[[1]]$inspect()$status$container_status$container_id
  container <- cl$containers$get(container_id)
  e <- container$exec(c("cat", sprintf("/%s", name)))
  log <- e$start(detach = FALSE, stream = FALSE)
  expect_identical(as.character(log), data)

  ans$remove()
})