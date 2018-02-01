context("docker client: volumes")

test_that("create", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  v <- d$volumes$create(nm)
  expect_is(v, "docker_volume")
  expect_is(v, "stevedore_object")

  expect_equal(v$name(), nm)
  expect_equal(v$inspect()$name, nm)
  expect_identical(v$reload(), v)

  expect_null(v$remove())

  e <- get_error(v$inspect())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("get", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  v1 <- d$volumes$create(nm)
  v2 <- d$volumes$get(nm)
  expect_identical(v1$inspect(FALSE), v2$inspect(FALSE))
  d$volumes$remove(nm)

  e <- get_error(d$volumes$get(nm))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("list", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  v <- d$volumes$create(nm)

  vl <- d$volumes$list()
  expect_is(vl, "data.frame")
  expect_true("name" %in% names(vl))
  expect_true(nm %in% vl$name)
})

test_that("map", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  v <- d$volumes$create(nm)
  on.exit(v$remove())
  expect_equal(v$map("/foo"), sprintf("%s:/foo", nm))
  expect_equal(v$map("/foo", TRUE), sprintf("%s:/foo:ro", nm))
  expect_error(v$map(1), "'path' must be a scalar character (non-NA)",
               fixed = TRUE)
})

test_that("prune", {
  d <- test_docker_client()
  ans <- d$volumes$prune()
  expect_match(ans$volumes_deleted, "^stevedore_", all = FALSE)
})
