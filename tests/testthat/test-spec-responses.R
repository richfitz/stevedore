context("spec (responses)")

## Known issues:
##
## image_inspect is producing a rogue
##   volumes = list(additional_properties = NULL)
## network_inspect$i_pam$options is not right, but that looks like we're not trimming names correctly

test_that("object, atomic scalar components (system_ping)", {
  dat <- read_sample_response("sample_responses/v1.29/system_ping.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
})

test_that("null (container_start)", {
  dat <- read_sample_response("sample_responses/v1.29/container_start.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
})

test_that("object, atomic scalar components (system_auth)", {
  dat <- read_sample_response("sample_responses/v1.29/system_auth.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("object, array of array (container_top)", {
  dat <- read_sample_response("sample_responses/v1.29/container_top.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("object of objects (volume_inspect)", {
  dat <- read_sample_response("sample_responses/v1.29/volume_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
  expect_equal(names(ans1$status), pascal_to_snake(names(ans2$Status)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("complex object (volume_list)", {
  dat <- read_sample_response("sample_responses/v1.29/volume_list.R")
  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("complex objects (network_list)", {
  dat <- read_sample_response("sample_responses/v1.29/network_list.R")
  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)[[1]]), names(ans2)),
               character(0))
})

test_that("complex objects (container_inspect)", {
  dat <- read_sample_response("sample_responses/v1.29/container_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
  skip("network_settings still not right")
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
  dat <- read_sample_response("sample_responses/v1.29/system_df.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("auto: container_changes", {
  dat <- read_sample_response("sample_responses/v1.29/container_changes.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)[[1]]), names(ans2)),
               character(0))
})

test_that("auto: container_create", {
  dat <- read_sample_response("sample_responses/v1.29/container_create.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("auto: container_list", {
  dat <- read_sample_response("sample_responses/v1.29/container_list.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)[[1]]), names(ans2)),
               character(0))
})

test_that("auto: container_stats", {
  dat <- read_sample_response("sample_responses/v1.29/container_stats.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: exec_inspect", {
  dat <- read_sample_response("sample_responses/v1.29/exec_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  msg <- setdiff(names(raw_to_json(dat$response)),
                 c(names(ans2), "CanRemove", "DetachKeys"))
  expect_equal(msg, character(0))

  ## This was incorrectly NULL because pick was not picky enough
  str <- sub('"ExitCode":2', '"ExitCode":null', rawToChar(dat$response))
  ans3 <- dat$handler(charToRaw(str), FALSE)
  expect_equal(ans3$exit_code, NA_integer_)
})

test_that("auto: image_delete", {
  dat <- read_sample_response("sample_responses/v1.29/image_delete.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)[[1]]), names(ans2)),
               character(0))
})

test_that("auto: image_history", {
  dat <- read_sample_response("sample_responses/v1.29/image_history.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)[[1]]), names(ans2)),
               character(0))
})

test_that("auto: image_inspect", {
  dat <- read_sample_response("sample_responses/v1.29/image_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  expect_equal(setdiff(names(raw_to_json(dat$response)), names(ans2)),
               character(0))
})

test_that("auto: image_list", {
  dat <- read_sample_response("sample_responses/v1.29/image_list.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: image_search", {
  dat <- read_sample_response("sample_responses/v1.29/image_search.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: network_create", {
  dat <- read_sample_response("sample_responses/v1.29/network_create.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: network_inspect", {
  dat <- read_sample_response("sample_responses/v1.29/network_inspect.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: system_events", {
  dat <- read_sample_response("sample_responses/v1.29/system_events.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: system_info", {
  dat <- read_sample_response("sample_responses/v1.29/system_info.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("auto: system_version", {
  dat <- read_sample_response("sample_responses/v1.29/system_version.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake_cached(names(ans2)))
})

test_that("auto: volume_create", {
  dat <- read_sample_response("sample_responses/v1.29/volume_create.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("all spec files read", {
  txt <- readLines("test-spec-responses.R")
  re <- '^.*read_sample_response\\("([^)]+)"\\).*'
  used <- sub(re, "\\1", grep(re, txt, value = TRUE))
  msg <- setdiff(dir("sample_responses", full.names = TRUE, pattern = "\\.R$"),
                 used)
  expect_equal(msg, character(0))
})

test_that("auto: containe_prune", {
  dat <- read_sample_response("sample_responses/v1.29/container_prune.R")

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))

  bytes <- charToRaw("{\"ContainersDeleted\":null,\"SpaceReclaimed\":0}")
  ans3 <- dat$handler(bytes, FALSE)
  expect_equal(ans3, list(containers_deleted = character(0),
                          space_reclaimed = 0L))
})

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

  ans1 <- dat$handler(dat$response, FALSE)
  ans2 <- dat$handler(dat$response, TRUE)

  expect_equal(ans1, dat$reference)
  expect_equal(ans2, dat$reference, check.attributes = FALSE)
  expect_equal(names(ans1), pascal_to_snake(names(ans2)))
})

test_that("system_df is a problem for unpacking of lists", {
  dat <- read_sample_response("sample_responses/regression/system_df_1.R")
  ans <- dat$handler(dat$response, FALSE)
  expect_equal(ans, dat$reference)

  dat <- read_sample_response("sample_responses/regression/system_df_2.R")
  ans <- dat$handler(dat$response, FALSE)
  expect_equal(ans, dat$reference)
})


test_that("header handler", {
  spec <- swagger_spec_read(DOCKER_API_VERSION_DEFAULT)
  response <- spec$paths[["/_ping"]][["get"]]$responses[["200"]]
  handler <- swagger_header_handler(response, spec)

  expect_equal(handler("Api-Version: 1.29\nDocker-Experimental: true", FALSE),
               list("api_version" = "1.29",
                    "docker_experimental" = TRUE))
  expect_equal(handler("other: 1", FALSE),
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
