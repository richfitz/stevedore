context("http (compatibility)")

test_that("streaming raw", {
  dc <- test_docker_client()
  dh <- test_docker_client(type = "httppipe")

  x1 <- dc$containers$create("richfitz/iterate", c("1000", "1"))
  on.exit(x1$remove(force = TRUE))
  x1$start()

  x2 <- dh$containers$get(x1$id())

  logs1 <- x1$logs()
  logs2 <- x2$logs()

  expect_true(all(logs1 %in% logs2))
})

test_that("build", {
  dc <- test_docker_client()
  context <- tar_directory("images/iterate")
  expect_output(dc$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = stdout(),
                                tag = "richfitz/iterate:testing"),
                "FROM alpine:latest")
  expect_silent(dc$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = NULL,
                                tag = "richfitz/iterate:testing"))
  p <- tempfile()
  expect_silent(dc$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = p,
                                tag = "richfitz/iterate:testing"))
  expect_match(readLines(p), "FROM alpine:latest", all = FALSE)

  dh <- test_docker_client(type = "httppipe")

  expect_output(dh$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = stdout(),
                                tag = "richfitz/iterate:testing"),
                "FROM alpine:latest")
  expect_silent(dh$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = NULL,
                                tag = "richfitz/iterate:testing"))
  p <- tempfile()
  expect_silent(dh$images$build(context, nocache = TRUE, rm = TRUE,
                                stream = p,
                                tag = "richfitz/iterate:testing"))
  expect_match(readLines(p), "FROM alpine:latest", all = FALSE)
})

test_that("docker run", {
  dh <- test_docker_client(type = "httppipe")
  expect_output(
    ans <- dh$containers$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                             rm = TRUE, stream = stdout()),
    "Doing 10 iterations")
  expect_silent(
    ans <- dh$containers$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                             rm = TRUE, stream = FALSE))
  p <- tempfile()
  expect_silent(
    ans <- dh$containers$run("richfitz/iterate", c("10", "0"), detach = FALSE,
                             rm = TRUE, stream = p))
  expect_match(readLines(p), "Doing 10 iterations", all = FALSE)
})

test_that("exec", {
  dh <- test_docker_client(type = "httppipe")
  nm <- rand_str(10, "stevedore_")
  ## this sets up a container that will run forever
  x <- dh$containers$create("richfitz/iterate",
                            cmd = c("100", "100"),
                            name = nm)
  x$start()

  e1 <- x$exec("ls")
  txt1 <- capture.output(res1 <- e1$start(detach = FALSE))
  expect_is(res1, "docker_stream")
  expect_equal(txt1, strsplit(format(res1, style = "prefix"), "\n")[[1]])

  e2 <- x$exec("ls")
  expect_silent(res2 <- e2$start(detach = FALSE, stream = FALSE))
  expect_is(res2, "docker_stream")
  expect_equal(format(res2), format(res1))

  e3 <- x$exec("ls")
  tmp <- tempfile()
  expect_silent(res3 <- e3$start(detach = FALSE, stream = tmp))
  expect_is(res3, "docker_stream")
  expect_equal(format(res3), format(res1))
  expect_equal(readLines(tmp), txt1)
})

test_that("pull", {
  skip_if_no_internet()
  dc <- test_docker_client(type = "curl")
  dh <- test_docker_client(type = "httppipe")

  txt0 <- capture.output(img0 <- dc$images$pull("alpine:latest"))
  str <- "Pulling from library/alpine latest"
  if (!any(grepl(str, txt0, fixed = TRUE))) {
    skip("Docker has changed pull text")
  }

  txt1 <- capture.output(img1 <- dh$images$pull("alpine:latest"))
  expect_match(txt1, str, fixed = TRUE, all = FALSE)

  expect_silent(img2 <- dh$images$pull("alpine:latest", stream = NULL))

  tmp <- tempfile()
  expect_silent(img3 <- dh$images$pull("alpine:latest", stream = tmp))
  expect_match(readLines(tmp), str, fixed = TRUE, all = FALSE)
})

test_that("log follow does not work", {
  dh <- test_docker_client(type = "httppipe")
  nm <- rand_str(10, "stevedore_")
  x <- dh$containers$create("richfitz/iterate", c("1000", "1"), name = nm)
  on.exit(x$remove())
  expect_error(
    x$logs(follow = TRUE),
    "Endpoint 'container_logs' cannot be implemented because the 'httppipe' http client does not currently support streaming connections", fixed = TRUE)
})
