context("docker client")

## The most simple nontrivial thing with the docker client
test_that("ping", {
  d <- docker_client()
  expect_is(d, "stevedore_object")
  expect_is(d, "docker_client")
  ok <- d$ping()
  expect_equivalent(ok, "OK")
  expect_equal(names(attributes(ok)),
               c("api_version", "docker_experimental"))
})

test_that("info", {
  d <- docker_client()
  info <- d$info()
  expect_is(info, "list")
})

test_that("version", {
  d <- docker_client()
  v <- d$version()
  expect_is(v, "list")
  expect_is(v$api_version, "character")
})

test_that("df", {
  skip("df containers output looks broken to me")
  d <- docker_client()
  df <- d$df()
})

test_that("events", {
  skip("events this needs some streaming support")
  d <- docker_client()
})

test_that("children", {
  d <- docker_client()
  expect_is(d$containers, "docker_container_collection")
  expect_is(d$containers, "stevedore_object")

  expect_is(d$images, "docker_image_collection")
  expect_is(d$images, "stevedore_object")

  expect_is(d$networks, "docker_network_collection")
  expect_is(d$networks, "stevedore_object")

  expect_is(d$volumes, "docker_volume_collection")
  expect_is(d$volumes, "stevedore_object")
})

test_that("Prevent invalid access", {
  d <- docker_client()
  expect_error(d$foo, "No element 'foo' within 'docker_client' object")
  expect_error(d[["foo"]], "No element 'foo' within 'docker_client' object")
  expect_error(d[[1]], "'i' must be a character")

  expect_error(d$containers$foo,
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d[["containers"]][["foo"]],
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d$containers[[1]], "'i' must be a character")
})
