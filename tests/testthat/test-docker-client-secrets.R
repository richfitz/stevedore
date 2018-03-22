context("secrets")

test_that("create and delete", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  ## This set of tests is highly version specific with at three
  ## patches so I am going to test it over all supported versions:
  for (v in swagger_spec_versions()) {
    cl_v <- test_docker_client(api_version = v)

    key <- rand_str()
    id <- cl$secrets$create(key, "secret!")
    expect_is(id, "character")
    expect_true(id %in% cl$secrets$list()$id)
    expect_true(key %in% cl$secrets$list()$name)

    dat <- cl$secrets$inspect(id)
    expect_identical(cl$secrets$inspect(key), dat)

    expect_null(cl$secrets$remove(key))
    expect_false(id %in% cl$secrets$list()$id)
    expect_false(key %in% cl$secrets$list()$name)
  }
})


test_that("add to container", {
  cl <- test_docker_client()

  cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  name <- "mysecret"
  data <- "secret!"
  id <- cl$secrets$create(name, data)

  ans <- cl$services$create(name = "redis",
                            image = "redis",
                            secrets = name,
                            timeout = 20,
                            time_wait_stable = 0,
                            stream = NULL)

  tasks <- ans$tasks(list("desired-state" = "running"))
  expect_equal(length(tasks), 1L)

  container_id <- tasks[[1]]$inspect()$status$container_status$container_id
  container <- cl$containers$get(container_id)
  e <- container$exec(c("cat", sprintf("/run/secrets/%s", name)))
  log <- e$start(detach = FALSE, stream = FALSE)
  expect_identical(as.character(log), data)

  ans$remove()
})
