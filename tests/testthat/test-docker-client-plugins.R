context("docker client: plugins")

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

  for (v in c(MIN_DOCKER_API_VERSION,
              DEFAULT_DOCKER_API_VERSION,
              MAX_DOCKER_API_VERSION)) {
    cl <- test_docker_client(api_version = v)
    key <- rand_str()
    x <- cl$plugins$install("vieux/sshfs:latest", alias = key,
                            grant_all = TRUE, stream = FALSE)
    on.exit(try(x$remove(TRUE)))

    expect_is(x, "docker_plugin")
    expect_equal(x$name(), paste0(key, ":latest"))
    expect_is(x$id(), "character")
    expect_true(x$is_enabled())

    dat <- cl$plugins$list()
    expect_true(x$id() %in% dat$id)
    expect_true(x$name() %in% dat$name)

    expect_true(dat$enabled[match(x$name(), dat$name)])
    expect_null(x$disable())
    expect_false(x$is_enabled())
    dat <- cl$plugins$list()
    expect_false(dat$enabled[match(x$name(), dat$name)])

    expect_null(x$enable())
    expect_true(x$is_enabled())
    dat <- cl$plugins$list()
    expect_true(dat$enabled[match(x$name(), dat$name)])

    expect_null(x$remove(TRUE))
    on.exit()
  }
})


test_that("get (offline)", {
  cl <- null_docker_client()
  x <- cl$plugins$get(dummy_id())
  expect_is(x, "docker_plugin")
  expect_equal(x$name(), dummy_id())
})


test_that("build", {
  cl <- test_docker_client()
  key <- rand_str()
  res <- cl$plugins$create(key, "plugin-example")
  on.exit(res$remove())

  expect_is(res, "docker_plugin")
  expect_equal(res$name(), paste0(key, ":latest"))
  expect_false(res$is_enabled())
})
