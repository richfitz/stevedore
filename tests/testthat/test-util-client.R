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

test_that("pull setatus: no print", {
  path <- tempfile()
  p <- pull_status_printer(NULL)

  txt <- readLines("sample_responses/pull/ubuntu")
  expect_silent(for (i in txt) p(from_json(i)))
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


test_that("parse image", {
  expect_equal(parse_image_name("foo"),
               list(repo = NULL, name = "foo", image = "foo", tag = NULL))
  expect_equal(parse_image_name("foo:latest"),
               list(repo = NULL, name = "foo", image = "foo", tag = "latest"))

  expect_equal(parse_image_name("myrepo.net/foo"),
               list(repo = "myrepo.net", name = "foo",
                    image = "myrepo.net/foo", tag = NULL))
  expect_equal(parse_image_name("myrepo.net/foo:latest"),
               list(repo = "myrepo.net", name = "foo",
                    image = "myrepo.net/foo", tag = "latest"))

  expect_equal(parse_image_name("myrepo.net:5000/foo"),
               list(repo = "myrepo.net:5000", name = "foo",
                    image = "myrepo.net:5000/foo", tag = NULL))
  expect_equal(parse_image_name("myrepo.net:5000/foo:latest"),
               list(repo = "myrepo.net:5000", name = "foo",
                    image = "myrepo.net:5000/foo", tag = "latest"))

  expect_error(
    parse_image_name("foo:bar:baz"),
    "'foo:bar:baz' does not match pattern '[<repo>/]<image>[:<tag>]'",
    fixed = TRUE)
})

test_that("validate image and tag", {
  ## Check that names propagate on error:
  img <- "foo:latest"
  t <- "3.1"
  expect_error(validate_image_and_tag(img, t),
               "If 'img' includes a tag, then 't' must be NULL")

  expect_equal(validate_image_and_tag("foo:xxx", NULL),
               list(repo = NULL, name = "foo", image = "foo", tag = "xxx"))
  expect_equal(validate_image_and_tag("foo", NULL),
               list(repo = NULL, name = "foo", image = "foo", tag = "latest"))
  expect_equal(validate_image_and_tag("foo", "3.1"),
               list(repo = NULL, name = "foo", image = "foo", tag = "3.1"))
})
