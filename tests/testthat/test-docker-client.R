context("docker client")

test_that("unknown args prevented", {
  expect_error(docker_client(foo = "bar"),
               "Unknown argument passed to 'docker_client': foo")
})

## The most simple nontrivial thing with the docker client
test_that("ping", {
  d <- test_docker_client()
  expect_is(d, "stevedore_object")
  expect_is(d, "docker_client")
  ok <- d$ping()
  expect_equivalent(ok, "OK")
  expect_equal(names(attributes(ok)),
               c("api_version", "docker_experimental"))
})

test_that("info", {
  d <- test_docker_client()
  info <- d$info()
  expect_is(info, "list")
})

test_that("version", {
  d <- test_docker_client()
  v <- d$version()
  expect_is(v, "list")
  expect_is(v$api_version, "character")
})

test_that("df", {
  skip("df containers output looks broken to me")
  d <- test_docker_client()
  df <- d$df()
})

test_that("events", {
  skip("events this needs some streaming support")
  d <- test_docker_client()
})

test_that("children", {
  d <- null_docker_client()
  expect_is(d$containers, "docker_container_collection")
  expect_is(d$containers, "stevedore_object")

  expect_is(d$images, "docker_image_collection")
  expect_is(d$images, "stevedore_object")

  expect_is(d$networks, "docker_network_collection")
  expect_is(d$networks, "stevedore_object")

  expect_is(d$volumes, "docker_volume_collection")
  expect_is(d$volumes, "stevedore_object")

  expect_is(d$swarm, "docker_swarm_collection")
  expect_is(d$swarm, "stevedore_object")

  expect_is(d$nodes, "docker_node_collection")
  expect_is(d$nodes, "stevedore_object")

  expect_is(d$services, "docker_service_collection")
  expect_is(d$services, "stevedore_object")

  expect_is(d$tasks, "docker_task_collection")
  expect_is(d$tasks, "stevedore_object")

  expect_is(d$secrets, "docker_secret_collection")
  expect_is(d$secrets, "stevedore_object")

  expect_is(d$configs, "docker_config_collection")
  expect_is(d$configs, "stevedore_object")

  expect_is(d$plugins, "docker_plugin_collection")
  expect_is(d$plugins, "stevedore_object")
})

test_that("Prevent invalid access", {
  d <- null_docker_client()
  expect_error(d$foo, "No element 'foo' within 'docker_client' object")
  expect_error(d[["foo"]], "No element 'foo' within 'docker_client' object")
  expect_error(d[[1]], "'i' must be a scalar character (non-NA)", fixed = TRUE)

  expect_error(d$containers$foo,
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d[["containers"]][["foo"]],
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d$containers[[1]], "'i' must be a scalar character (non-NA)",
               fixed = TRUE)
})

test_that("print", {
  d <- null_docker_client()
  out <- capture.output(x <- print(d))
  expect_identical(x, d)
  expect_true(any(out == "<docker_client>"))
  expect_true(any(out == "  ping()"))

  out <- capture.output(x <- print(d$containers))
  expect_identical(x, d$containers)
  expect_true(any(out == "<docker_container_collection>"))
  expect_true(any(out == "  get(id)"))
})

test_that("api_version", {
  d <- null_docker_client()
  expect_equal(d$api_version(), DEFAULT_DOCKER_API_VERSION)
})

test_that("print endpoints", {
  d <- null_docker_client()
  expect_is(d$ping, "docker_client_method")
  txt <- capture.output(print(d$ping))
  expect <- c(
    "function()",
    "----------",
    "Ping. This is a dummy endpoint you can use to test if the server is",
    "  accessible.")
  expect_equal(txt, expect)

  txt <- capture.output(print(d$login))
  expect_match(txt, "Check auth configuration",
               all = FALSE, fixed = TRUE)
  expect_match(txt, "  email: For authentication to check",
               all = FALSE, fixed = TRUE)

  txt <- capture.output(print(d$events))
  expect_match(
    txt,
    "        - `type=<string>` object to filter by, one of `container`,",
    all = FALSE, fixed = TRUE)

  txt <- capture.output(print(d$login))
  expect_match(
    txt,
    "Check auth configuration. Validate credentials for a registry and, if",
    all = FALSE, fixed = TRUE)

  txt <- capture.output(print(d$containers$create))
  expect_match(
    txt,
    "  shell: Shell for when `RUN`, `CMD`, and `ENTRYPOINT` uses a shell",
    all = FALSE, fixed = TRUE)
})

test_that("login", {
  skip("not yet tested")
})

test_that("events", {
  skip("not yet tested")
})
