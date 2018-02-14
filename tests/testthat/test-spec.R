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
    expect_equal(attr(dat, "version"), v)
    expected <- vcapply(docker_api_client_endpoints(v), "[[", "name")
    expect_true(all(expected %in% names(dat)))
  }
})


## This just checks that spec upgrades should be easy:
test_that("spec check", {
  spec_25 <- swagger_spec_read("1.25")
  spec_33 <- swagger_spec_read("1.33")

  endpoints_25 <- docker_api_client_endpoints("1.25")
  endpoints_33 <- docker_api_client_endpoints("1.33")

  expect_silent(docker_api_client_data_check(spec_25, endpoints_25))
  expect_silent(docker_api_client_data_check(spec_33, endpoints_33))

  expect_message(docker_api_client_data_check(spec_33, endpoints_25),
                 "Unimplemented endpoints")
  expect_error(docker_api_client_data_check(spec_25, endpoints_33),
               "Unknown endpoints")
})
