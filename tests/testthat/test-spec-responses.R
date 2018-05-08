context("spec (responses)")


## The spec is incorrect about the PortBinding type which is used in
##
##     GET /containers/{id}/json => NetworkSettings => Ports
##
## See:
##
##   raw_to_json(dat$response)$NetworkSettings$Ports
##   dat$reference$network_settings$ports
##
## There are other issues with the spec but this one was causing
## problems that weren't silent.
test_that("regression: container_inspect", {
  ## In this case the json is exactly the response returned from the
  dat <- read_sample_response("sample_responses/regression/container_inspect.R")

  ans1 <- dat$handler(dat$response,
                      list(as_is_names = FALSE, data_frame = identity))
  ans2 <- dat$handler(dat$response,
                      list(as_is_names = TRUE, data_frame = identity))

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})


test_that("system_df is a problem for unpacking of lists", {
  dat <- read_sample_response("sample_responses/regression/system_df_1.R")
  ans <- dat$handler(dat$response, list(as_is_names = FALSE, data_frame = identity))
  expect_equal(ans, dat$reference)

  dat <- read_sample_response("sample_responses/regression/system_df_2.R")
  ans <- dat$handler(dat$response, list(as_is_names = FALSE, data_frame = identity))
  expect_equal(ans, dat$reference)
})


test_that("header handler", {
  spec <- swagger_spec_read(DOCKER_API_VERSION_DEFAULT)
  response <- spec$paths[["/_ping"]][["get"]]$responses[["200"]]
  handler <- swagger_header_handler(response, spec)

  opts <- list(as_is_names = FALSE, data_frame = identity)

  expect_equal(handler("Api-Version: 1.29\nDocker-Experimental: true", opts),
               list("api_version" = "1.29",
                    "docker_experimental" = TRUE))
  expect_equal(handler("other: 1", opts),
               list("api_version" = NA_character_,
                    "docker_experimental" = NA))
})


test_that("binary response handler", {
  h <- swagger_response_handler_binary()
  expect_identical(h(NULL), NULL)
  expect_identical(h(raw(100)), raw(100))
})


test_that("chunked string handler", {
  h <- swagger_response_handler_chunked_string()
  x <- read_binary("sample_responses/logs/simple")

  expect_identical(h(x), decode_chunked_string(x))
  expect_identical(h(charToRaw(paste(letters, collapse = ""))),
                   paste(letters, collapse = ""))
})
