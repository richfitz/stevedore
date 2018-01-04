context("util (client)")

test_that("report warnings", {
  expect_silent(report_warnings(character(0), "doing a thing"))
  expect_warning(report_warnings("my warning", "doing a thing"),
                 "1 warning produced while doing a thing.+- my warning")
  expect_warning(report_warnings(c("my warning", "another warning"),
                                 "doing a thing"),
                 paste0("2 warnings produced while doing a thing.+",
                        "- my warning.+",
                        "- another warning"))
})

test_that("pull status", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- pull_status_printer(con)

  txt <- readLines("sample_responses/pull/ubuntu")
  cmp <- readLines("sample_responses/pull/ubuntu.out")
  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  expect_equal(res, cmp)
})

test_that("pull status: silent error", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- pull_status_printer(con)

  txt <- readLines("sample_responses/pull/hello-world-macos")
  cmp <- readLines("sample_responses/pull/hello-world-macos.out")
  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  expect_equal(res, cmp)
})

test_that("validate volumes", {
  expect_null(validate_volumes(NULL))
  expect_null(validate_volumes(character()))

  expect_equal(validate_volumes("foo:bar"),
               list(binds = "foo:bar", volumes = list("foo" = NULL)))
  expect_equal(validate_volumes(c("foo:bar", "/a/b:/c/d:ro")),
               list(binds = c("foo:bar", "/a/b:/c/d:ro"),
                    volumes = list("foo" = NULL, "/a/b" = NULL)))

  expect_error(validate_volumes("foo"),
               "Volume mapping 'foo' does not not match '<src>:<dest>[:ro]",
               fixed = TRUE)
  expect_error(
    validate_volumes(c("foo", "bar")),
    "Volume mapping 'foo', 'bar' does not not match '<src>:<dest>[:ro]",
    fixed = TRUE)
  expect_error(
    validate_volumes(c("foo", "a:b", "bar")),
    "Volume mapping 'foo', 'bar' does not not match '<src>:<dest>[:ro]",
    fixed = TRUE)
})

test_that("validate ports", {
  expect_null(validate_ports(NULL))
  expect_null(validate_ports(character()))

  expect_equal(validate_ports("22:33"),
               list(port_bindings = list(
                      "33/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox("22")))),
                    ports = list("33/tcp" = NULL)))

  ## Check serialisation:
  str <-
    as.character(jsonlite::toJSON(validate_ports("11022:22")$port_bindings))
  cmp <- '{"22/tcp":[{"HostIp":"","HostPort":"11022"}]}'
  expect_identical(str, cmp)

  expect_error(validate_ports(""),
               "Port binding '' does not not match '<host>:<container>")
  expect_error(validate_ports("111x"),
               "Port binding '111x' does not not match '<host>:<container>")
})

test_that("validate ports: random", {
  expect_equal(validate_ports("80"),
               list(port_bindings = list(
                      "80/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox("")))),
                    ports = list("80/tcp" = NULL)))
  expect_equal(validate_ports(c("80", "90")),
               list(port_bindings = list(
                      "80/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox(""))),
                      "90/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox("")))),
                    ports = list("80/tcp" = NULL, "90/tcp" = NULL)))
  expect_equal(validate_ports(c("80", "100:90")),
               list(port_bindings = list(
                      "80/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox(""))),
                      "90/tcp" = list(list(
                        HostIp = jsonlite::unbox(""),
                        HostPort = jsonlite::unbox("100")))),
                    ports = list("80/tcp" = NULL, "90/tcp" = NULL)))
})
