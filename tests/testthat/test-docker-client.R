context("docker client")

## The most simple nontrivial thing with the docker client
test_that("construction", {
  cl <- docker_client(api_version)
  expect_is(cl, "stevedore_object")
  expect_is(cl, "docker_client")
  ok <- cl$ping()
  expect_equivalent(ok, "OK")
  expect_equal(names(attributes(ok)),
               c("api_version", "docker_experimental"))
})
