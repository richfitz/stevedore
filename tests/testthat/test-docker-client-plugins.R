context("docker client: plugins")

PLUGIN_SSHFS <- "vieux/sshfs:latest"

test_that("privileges", {
  skip_if_no_internet()
  cl <- test_docker_client()
  dat <- cl$plugins$privileges("vieux/sshfs:latest")
  ## TODO: I think that this would benefit from being a classed object
  ## perhaps?  Something that can be agreed to?
  expect_is(dat, "data.frame")
})


test_that("install", {
  skip_if_no_internet()

  cl <- test_docker_client()
  key <- rand_str()
  x <- cl$plugins$install("vieux/sshfs:latest", alias = key, grant_all = TRUE)
  on.exit(try(x$remove(TRUE)))

  expect_is(x, "docker_plugin")
  expect_equal(x$name(), paste0(key, ":latest"))
  expect_is(x$id(), "character")

  dat <- cl$plugins$list()
  expect_true(x$id() %in% dat$id)
  expect_true(x$name() %in% dat$name)

  expect_true(dat$enabled[match(x$name(), dat$name)])
  expect_null(x$disable())
  expect_false(dat$enabled[match(x$name(), dat$name)])
  expect_null(x$enable())
  expect_true(dat$enabled[match(x$name(), dat$name)])

  expect_null(x$remove(TRUE))
  on.exit()
})


test_that("get (offline)", {
  cl <- null_docker_client()
  x <- cl$plugins$get(dummy_id())
  expect_is(x, "docker_plugin")
  expect_equal(x$name(), dummy_id())
})
