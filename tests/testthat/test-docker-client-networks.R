context("docker client: networks")

test_that("create", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  nw <- d$networks$create(nm)
  expect_is(nw, "docker_network")
  expect_is(nw, "stevedore_object")

  expect_equal(nw$name(), nm)
  expect_equal(nw$inspect()$name, nm)
  expect_identical(nw$reload(), nw)

  expect_null(nw$remove())

  e <- get_error(nw$inspect())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("get", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  nw1 <- d$networks$create(nm)
  nw2 <- d$networks$get(nm)
  expect_identical(nw1$inspect(FALSE), nw2$inspect(FALSE))
  d$networks$remove(nm)

  e <- get_error(d$networks$get(nm))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("list", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  nw <- d$networks$create(nm)

  nwl <- d$networks$list()
  expect_is(nwl, "data.frame")
  expect_true("name" %in% names(nwl))
  expect_true(nm %in% nwl$name)
})

test_that("prune", {
  d <- docker_client()
  ans <- d$networks$prune()
  expect_match(ans$networks_deleted, "^stevedore_", all = FALSE)
})
