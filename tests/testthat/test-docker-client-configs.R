context("configs")

test_that("create and delete", {
  docker_versions <- test_docker_versions()
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  for (v in docker_versions) {
    cl_v <- docker_client(api_version = v)

    key <- rand_str()

    if (numeric_version(v) < numeric_version("1.30")) {
      expect_error(
        cl_v$config$create(key, "config"),
        "'config_create' (POST /configs/create) requires docker API version",
        fixed = TRUE)
      next
    }

    id <- cl_v$config$create(key, "config!")
    expect_is(id, "character")
    expect_true(id %in% cl_v$config$list()$id)
    expect_true(key %in% cl_v$config$list()$name)

    dat <- cl_v$config$inspect(id)
    expect_identical(cl_v$config$inspect(key), dat)

    expect_null(cl_v$config$remove(key))
    expect_false(id %in% cl_v$config$list()$id)
    expect_false(key %in% cl_v$config$list()$name)
  }
})


test_that("add to container", {
  cl <- test_docker_client(api_version = "1.30")

  cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  name <- "myconfig"
  data <- "config!"
  id <- cl$config$create(name, data)

  ans <- cl$service$create(name = "redis",
                           image = "redis",
                           configs = name,
                           timeout = 20,
                           time_wait_stable = 0,
                           stream = NULL)

  tasks <- ans$tasks(list("desired-state" = "running"))
  expect_equal(length(tasks), 1L)

  container_id <- tasks[[1]]$inspect()$status$container_status$container_id
  container <- cl$container$get(container_id)
  log <- container$exec(c("cat", sprintf("/%s", name)), stream = FALSE)
  expect_identical(as.character(log$output), data)

  ## This ensures that the daemon has time to clean up the temporary
  ## volume that it needs to use.
  ans$remove()
  wait_until_container_gone(container)
})
