context("docker client: containers")

test_that("create", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  expect_is(x, "docker_container")
  expect_is(x, "stevedore_object")

  expect_equal(x$name(), nm)
  expect_equal(x$inspect()$name, paste0("/", nm))
  expect_identical(x$reload(), x)

  expect_null(x$remove())

  e <- get_error(x$inspect())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("get", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  x1 <- d$containers$create("hello-world", name = nm)
  x2 <- d$containers$get(nm)
  expect_identical(x1$inspect(FALSE), x2$inspect(FALSE))

  d$containers$remove(nm)

  e <- get_error(d$containers$get(nm))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("list", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  cl <- d$containers$list(all = TRUE)
  expect_is(cl, "data.frame")
  expect_true("names" %in% names(cl))
  expect_true(list(nm) %in% cl$names)
  expect_true(nm %in% cl$name)
})

test_that("prune", {
  d <- docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  ans <- d$containers$prune()
  expect_true(x$id() %in% ans$containers_deleted)
})
