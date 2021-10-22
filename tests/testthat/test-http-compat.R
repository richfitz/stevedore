context("http (compatibility)")


test_that("streaming raw", {
  skip_if_no_httppipe_support()
  dc <- test_docker_client()
  dh <- test_docker_client(http_client_type = "httppipe")

  x1 <- dc$container$create("richfitz/iterate", c("1000", "1"))
  on.exit(x1$remove(force = TRUE))
  x1$start()

  x2 <- dh$container$get(x1$id())

  logs1 <- x1$logs()
  logs2 <- x2$logs()

  expect_true(all(logs1 %in% logs2))
})


test_that("build", {
  skip_if_no_httppipe_support()
  dc <- test_docker_client()
  context <- tar_directory("images/iterate")
  expect_output(dc$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = stdout(),
                               tag = "richfitz/iterate:testing"),
                "FROM alpine:latest")
  expect_silent(dc$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = NULL,
                               tag = "richfitz/iterate:testing"))
  p <- tempfile_test()
  expect_silent(dc$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = p,
                               tag = "richfitz/iterate:testing"))
  expect_match(readLines(p), "FROM alpine:latest", all = FALSE)

  dh <- test_docker_client(http_client_type = "httppipe")

  expect_output(dh$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = stdout(),
                               tag = "richfitz/iterate:testing"),
                "FROM alpine:latest")
  expect_silent(dh$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = NULL,
                               tag = "richfitz/iterate:testing"))
  p <- tempfile_test()
  expect_silent(dh$image$build(context, nocache = TRUE, rm = TRUE,
                               stream = p,
                               tag = "richfitz/iterate:testing"))
  expect_match(readLines(p), "FROM alpine:latest", all = FALSE)
})


test_that("docker run", {
  skip_if_no_httppipe_support()
  dh <- test_docker_client(http_client_type = "httppipe")
  expect_output(
    ans <- dh$container$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                            rm = TRUE, stream = stdout()),
    "Doing 10 iterations")
  expect_silent(
    ans <- dh$container$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                            rm = TRUE, stream = FALSE))
  p <- tempfile_test()
  expect_silent(
    ans <- dh$container$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                            rm = TRUE, stream = p))
  expect_match(readLines(p), "Doing 10 iterations", all = FALSE)
})


test_that("exec", {
  skip_if_no_httppipe_support()
  dh <- test_docker_client(http_client_type = "httppipe")
  nm <- rand_str(10, "stevedore_")
  ## this sets up a container that will run forever
  x <- dh$container$create("richfitz/iterate",
                           cmd = c("100", "100"),
                           name = nm)
  x$start()
  on.exit(x$remove(force = TRUE))

  txt1 <- capture.output(res1 <- x$exec("ls"))
  expect_is(res1$output, "docker_stream")

  cmp <- unlist(strsplit(format(res1$output, style = "prefix"), "\n"))
  expect_equal(txt1, cmp)

  expect_silent(res2 <- x$exec("ls", stream = FALSE))
  expect_is(res2$output, "docker_stream")
  expect_equal(format(res2$output), format(res1$output))

  tmp <- tempfile_test()
  expect_silent(res3 <- x$exec("ls", stream = tmp))
  expect_is(res3$output, "docker_stream")
  expect_equal(format(res3$output), format(res1$output))
  expect_equal(readLines(tmp), txt1)
})


test_that("pull", {
  skip_if_no_httppipe_support()
  skip_on_cran()
  skip_if_no_internet()
  dc <- test_docker_client(http_client_type = "curl")
  dh <- test_docker_client(http_client_type = "httppipe")

  txt0 <- capture.output(img0 <- dc$image$pull("alpine:latest"))
  str <- "Pulling from library/alpine latest"
  if (!any(grepl(str, txt0, fixed = TRUE))) {
    skip("Docker has changed pull text")
  }

  txt1 <- capture.output(img1 <- dh$image$pull("alpine:latest"))
  expect_match(txt1, str, fixed = TRUE, all = FALSE)

  expect_silent(img2 <- dh$image$pull("alpine:latest", stream = NULL))

  tmp <- tempfile_test()
  expect_silent(img3 <- dh$image$pull("alpine:latest", stream = tmp))
  expect_match(readLines(tmp), str, fixed = TRUE, all = FALSE)
})


test_that("log follow does not work", {
  skip_if_no_httppipe_support()
  dh <- test_docker_client(http_client_type = "httppipe")
  nm <- rand_str(10, "stevedore_")
  x <- dh$container$create("richfitz/iterate", c("1000", "1"), name = nm)
  on.exit(x$remove())
  expect_error(
    x$logs(follow = TRUE),
    "Endpoint 'container_logs' cannot be implemented because the 'httppipe' http client does not currently support streaming connections", fixed = TRUE)
})
