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
  expected <- vcapply(docker_api_client_endpoints(), "[[", "name")
  for (v in swagger_spec_versions()) {
    message(v)
    dat <- docker_api_client_data(v)
    expect_equal(attr(dat, "version"), v)
    expect_true(all(expected %in% names(dat)))
  }
})
