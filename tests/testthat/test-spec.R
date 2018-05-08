context("spec")


## This tests very little aside from things exist.
test_that("read", {
  for (v in swagger_spec_versions()) {
    dat <- swagger_spec_read(v)
    expect_equal(dat$info$version, v)
  }
})


## Then test that we can generate all the required bits.  This one
## really checks very little; just that we can create all the bits
## that would feed into the high level client functions.
test_that("build", {
  for (v in swagger_spec_versions()) {
    message(v)
    dat <- docker_api_client_data(v)
    expect_equal(dat$version, v)
    expected <- vcapply(docker_api_client_endpoints(), "[[", "name")
    expect_true(all(expected %in% names(dat$endpoints)))
  }
})


test_that("spec check", {
  spec <- swagger_spec_read("1.29")
  endpoints <- docker_api_client_endpoints()

  expect_silent(docker_api_client_data_check(spec, endpoints))

  expect_error(
    docker_api_client_data_check(spec, endpoints[-1]),
    "Unimplemented endpoints (stevedore bug)",
    fixed = TRUE)

  if (length(STEVEDORE_UNIMPLEMENTED) > 0L) {
    tmp <- endpoints[seq_along(STEVEDORE_UNIMPLEMENTED)]
    for (i in seq_along(STEVEDORE_UNIMPLEMENTED)) {
      m <- strsplit(STEVEDORE_UNIMPLEMENTED[[i]], " ")[[1]]
      tmp[[i]]$method <- m[[1]]
      tmp[[i]]$path <- m[[2]]
    }
    expect_error(
      docker_api_client_data_check(spec, c(endpoints, tmp)),
       "Unexpected implemented methods (stevedore bug):",
      fixed = TRUE)
  }
})


test_that("image build clean in old versions", {
  cl <- test_docker_client(api_version = "1.29")
  expect_error(
    cl$image$build_clean(),
    "'image_build_clean' (POST /build/prune) requires docker API version at least 1.31 (version 1.29 used)",
    fixed = TRUE)
})


test_that("spec into temporary dir", {
  oo <- options(stevedore.spec.path = NULL)
  on.exit(options(oo))

  expect_message(p1 <- swagger_spec_path(FALSE),
                 "The option 'stevedore.spec.path' not set")
  expect_equal(dir(p1),
               dir(stevedore_file("spec"), pattern = "v[0-9]+\\.[0-9]+.yaml$"))

  options(stevedore.spec.path = NULL)
  expect_silent(p2 <- swagger_spec_path(TRUE))
  expect_equal(dir(p1), dir(p2))

  unlink(p1, recursive = TRUE)
  unlink(p2, recursive = TRUE)
})
