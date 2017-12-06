context("spec")

## This tests very little aside from things exist.
test_that("read", {
  for (v in spec_versions()) {
    dat <- read_spec(v)
    expect_equal(dat$info$version, v)
  }
})

## Then test that we can generate all the required bits.  This one
## really checks very little; just that we can create all the bits
## that would feed into the high level client functions.
test_that("build", {
  expected <- vcapply(.stevedore$endpoints, "[[", "name")
  for (v in spec_versions()) {
    message(v)
    dat <- docker_client_data(v)
    expect_equal(dat$version, v)
    expect_true(all(expected %in% names(dat$endpoints)))
  }
})
