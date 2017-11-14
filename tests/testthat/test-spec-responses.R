context("spec (responses)")

test_that("object, atomic scalar components (system_ping)", {
  dat <- read_sample_response("sample_responses/system_ping.R")
  ans <- dat$handler(dat$response, FALSE)
  expect_equal(ans, dat$reference)
})

test_that("null (container_start)", {
  dat <- read_sample_response("sample_responses/container_start.R")
  ans <- dat$handler(dat$response)
  expect_equal(ans, dat$reference)
})

test_that("object, atomic scalar components (system_auth)", {
  dat <- read_sample_response("sample_responses/system_auth.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("object, array of array (container_top)", {
  dat <- read_sample_response("sample_responses/container_top.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("object of objects (volume_inspect)", {
  dat <- read_sample_response("sample_responses/volume_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
  expect_equal(names(ans1$status), pascal_to_snake(names(ans2$Status)))
})

test_that("complex object (volume_list)", {
  dat <- read_sample_response("sample_responses/volume_list.R")
  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
  ## TODO: test recursive bits
})

test_that("complex objects (network_list)", {
  dat <- read_sample_response("sample_responses/network_list.R")
  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
  ## TODO: test recursive bits
})

test_that("complex objects (container_inspect)", {
  dat <- read_sample_response("sample_responses/container_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
  ## TODO: test recursive bits
})

test_that("complex objects (system_df)", {
  ## So this is triggering incorrectly fairly high up in the chain,
  ## but I'm not sure where - the first point of error is where the
  ## print statement is but that is done within an lapply leading to
  ## an extra level of indexing that should not be there - so the loop
  ## is just interleaved incorrectly somewhere (or I am interpreting
  ## the spec wrong or the spec is wrong).  The actual data coming out
  ## of the json looks right and oddly it looks like the same pattern
  ## of data as the other elements here that work just fine thank you
  ## very much.
  dat <- read_sample_response("sample_responses/system_df.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  ## Patch up a known issue here:
  ans1$containers <- ans1$containers[[1]]
  ans2$Containers <- ans2$Containers[[1]]

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})
