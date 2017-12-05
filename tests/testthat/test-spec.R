context("spec")

## This tests very little aside from things exist.
test_that("read", {
  for (v in spec_versions()) {
    dat <- read_spec(v)
    expect_equal(dat$info$version, v)
  }
})
