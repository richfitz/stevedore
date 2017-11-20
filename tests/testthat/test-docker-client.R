context("docker client")

## The most simple nontrivial thing with the docker client
test_that("construction", {
  docker <- docker_client(api_version)
  expect_is(docker, "stevedore_object")
  expect_is(docker, "docker_client")
  ok <- docker$ping()
  expect_equivalent(ok, "OK")
  expect_equal(names(attributes(ok)),
               c("api_version", "docker_experimental"))
})
