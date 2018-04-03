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


test_that("build status: success", {
  ## To generate these, pop a browser call into after_build
  ##   dir.create("sample_responses/build", FALSE, TRUE)
  ##   writeBin(x$response$content, "sample_responses/build/success")
  ##   writeBin(x$response$content, "sample_responses/build/failure")

  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- build_status_printer(con)

  txt <- readLines("sample_responses/build/success")
  cmp <- readLines("sample_responses/build/success.out")

  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  ## writeLines(res, "sample_responses/build/success.out")

  expect_equal(res, cmp)

  bin <- read_binary("sample_responses/build/success")
  expect_identical(build_status_id(bin), "8d9538fe3885")
})


test_that("build status: failure", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- build_status_printer(con)

  txt <- readLines("sample_responses/build/failure")
  cmp <- readLines("sample_responses/build/failure.out")

  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  ## writeLines(res, "sample_responses/build/failure.out")

  expect_equal(res, cmp)

  bin <- read_binary("sample_responses/build/failure")
  err <- get_error(build_status_id(bin))
  expect_is(err, "build_error")
})


test_that("streaming_text", {
  text <- character()
  callback <- function(x) {
    text <<- c(text, x)
  }

  p <- streaming_text(callback)
  expect_equal(hijacked_content(p), raw())

  p(charToRaw("hello"))
  expect_equal(text, "hello")
  expect_equal(hijacked_content(p), charToRaw("hello"))

  p(charToRaw("goodbye"))
  expect_equal(text, c("hello", "goodbye"))
  expect_equal(hijacked_content(p), charToRaw("hellogoodbye"))
})


test_that("streaming_json", {
  data <- list()
  callback <- function(x) {
    data <<- c(data, list(x))
  }

  p <- streaming_json(callback)
  expect_equal(hijacked_content(p), raw())

  p(charToRaw("[1,2,3]\r\n"))
  expect_equal(data, list(list(1, 2, 3)))
  expect_equal(hijacked_content(p), charToRaw("[1,2,3]\r\n"))

  p(charToRaw('"a"\r\n{"x":1}\r\n'))
  expect_equal(data, list(list(1, 2, 3), "a", list(x = 1)))
  expect_equal(hijacked_content(p), charToRaw('[1,2,3]\r\n"a"\r\n{"x":1}\r\n'))
})


test_that("decode_chunked_string", {
  x <- read_binary("sample_responses/logs/simple")
  res <- decode_chunked_string(x)
  expect_is(res, "docker_stream")
  expect_equal(length(res), 5)
  expect_true(all(as.character(attr(res, "stream")) == "stdout"))
})


test_that("docker_stream_printer: null", {
  p <- docker_stream_printer(NULL)
  expect_silent(p("anything"))
})


test_that("docker_stream_printer: text", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- docker_stream_printer(con)

  txt <- c("these", "are some lines", "of text")
  expect_silent({
    for (i in txt) {
      p(i)
    }
  })
  close(con)
  on.exit()

  res <- readLines(path)
  unlink(path)
  expect_identical(res, txt)
})


test_that("docker_stream_printer: docker_stream", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- docker_stream_printer(con, "prefix")

  txt <- paste0(c("these", "are some lines", "of text"), "\n")
  obj <- docker_stream(txt, c(1, 2, 1))

  expect_silent(p(obj))
  close(con)
  on.exit()

  res <- readLines(path)
  unlink(path)
  expect_identical(res, format(obj, style = "prefix", strip_newline = TRUE))
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

test_that("pull status: no print", {
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

  expect_true(validate_ports(TRUE))
  expect_equal(validate_ports(c("111", "222")),
               validate_ports(c(111, 222)))
})


test_that("parse image", {
  expect_equal(parse_image_name("foo"),
               list(repo = NULL, name = "foo", image = "foo", tag = NULL,
                    registry = "docker.io"))
  expect_equal(parse_image_name("foo:latest"),
               list(repo = NULL, name = "foo", image = "foo", tag = "latest",
                    registry = "docker.io"))

  expect_equal(parse_image_name("repo/foo"),
               list(repo = "repo", name = "foo", image = "repo/foo",
                    tag = NULL, registry = "docker.io"))
  expect_equal(parse_image_name("repo/foo:latest"),
               list(repo = "repo", name = "foo", image = "repo/foo",
                    tag = "latest", registry = "docker.io"))

  expect_equal(parse_image_name("myrepo.net/foo"),
               list(repo = "myrepo.net", name = "foo",
                    image = "myrepo.net/foo", tag = NULL,
                    registry = "myrepo.net"))
  expect_equal(parse_image_name("myrepo.net/foo:latest"),
               list(repo = "myrepo.net", name = "foo",
                    image = "myrepo.net/foo", tag = "latest",
                    registry = "myrepo.net"))

  expect_equal(parse_image_name("myrepo.net:5000/foo"),
               list(repo = "myrepo.net:5000", name = "foo",
                    image = "myrepo.net:5000/foo", tag = NULL,
                    registry = "myrepo.net:5000"))
  expect_equal(parse_image_name("myrepo.net:5000/foo:latest"),
               list(repo = "myrepo.net:5000", name = "foo",
                    image = "myrepo.net:5000/foo", tag = "latest",
                    registry = "myrepo.net:5000"))

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
               list(repo = NULL, name = "foo", image = "foo", tag = "xxx",
                    registry = "docker.io"))
  expect_equal(validate_image_and_tag("foo", NULL),
               list(repo = NULL, name = "foo", image = "foo", tag = "latest",
                    registry = "docker.io"))
  expect_equal(validate_image_and_tag("foo", "3.1"),
               list(repo = NULL, name = "foo", image = "foo", tag = "3.1",
                    registry = "docker.io"))
})

test_that("validate stream", {
  d <- validate_stream(tempfile())
  expect_is(d$stream, "connection")
  expect_equal(summary(d$stream)$mode, "wb")
  expect_true(isOpen(d$stream))
  expect_true(d$close)
  expect_equal(validate_stream(d$stream),
               list(stream = d$stream, close = FALSE))
  close(d$stream)

  expect_equal(validate_stream(stdout()),
               list(stream = stdout(), close = FALSE))
  expect_equal(validate_stream(TRUE),
               list(stream = stdout(), close = FALSE))

  expect_equal(validate_stream(NULL),
               list(stream = NULL, close = FALSE))
  expect_equal(validate_stream(FALSE),
               list(stream = NULL, close = FALSE))
})

test_that("validate env", {
  expect_null(validate_env(NULL))
  expect_null(validate_env(character(0)))

  expect_equal(validate_env(c(a = "value")), "a=value")
  expect_equal(validate_env(c(a = "value", b = "another")),
               c("a=value", "b=another"))

  expect_equal(validate_env(list(a = "value", b = 10)),
               c("a=value", "b=10"))

  ## Unset:
  expect_equal(validate_env(c(a = NA)), "a=")
  expect_equal(validate_env(list(a = NULL)), "a=")
  expect_equal(validate_env(list(a = "")), "a=")

  expect_equal(validate_env(c(a = NA, b = "value")), c("a=", "b=value"))
  expect_equal(validate_env(list(a = NULL, b = "value")), c("a=", "b=value"))
  expect_equal(validate_env(list(a = "", b = "value")), c("a=", "b=value"))

  expect_error(validate_env(c("value", "another")),
               "must be named")
  expect_error(validate_env(c("a=value", "b=another")),
               "must be named")

  x <- list(a = 1, b = 2:3)
  expect_error(validate_env(x),
               "All elements of 'x' must be scalar (or use atomic vector)",
               fixed = TRUE)
})


test_that("nonapi methods", {
  fn <- function() ""
  expect_error(docker_client_method_nonapi(fn, "no", "method"),
               "missing help for no$method", fixed = TRUE)
  expect_error(docker_client_method_nonapi(fn, "docker_image", "help"),
               "incorrect help for docker_image$help", fixed = TRUE)
})


test_that("drop_leading_slash", {
  expect_equal(drop_leading_slash("/a/b"), "a/b")
  expect_equal(drop_leading_slash("a/b"), "a/b")
})


test_that("short_id", {
  expect_equal(short_id("abcdefghijklmnopqrstuvwxyz"),
               "abcdefghij")
  expect_equal(short_id("sha256:abcdefghijklmnopqrstuvwxyz"),
               "sha256:abcdefghij")
})


test_that("image_name_with_tag", {
  expect_equal(image_name_with_tag("foo"), "foo:latest")
  expect_equal(image_name_with_tag("foo:bar"), "foo:bar")

  expect_equal(image_name_with_tag("myrepo.net:5000/foo"),
               "myrepo.net:5000/foo:latest")
  expect_equal(image_name_with_tag("myrepo.net:5000/foo:bar"),
               "myrepo.net:5000/foo:bar")
})


test_that("decode_chunked_string", {
  ## TODO: harvest some real values here?
  expect_equal(decode_chunked_string(raw()), character())
})


test_that("get_image_id", {
  expect_equal(get_image_id("aa"), "aa")
  expect_equal(
    get_image_id(structure(list(id = function() "bb"), class = "docker_image")),
    "bb")
})


test_that("get_network_id", {
  expect_equal(get_network_id("aa"), "aa")
  expect_equal(
    get_network_id(structure(list(id = function() "bb"),
                             class = "docker_network")),
    "bb")
})


test_that("validate_tar", {
  path <- tempfile()
  writeLines("hello", path)
  res <- validate_tar_input(path)
  expect_is(res, "raw")
  expect_identical(validate_tar_input(res), res)

  tmp <- untar_bin(res)
  expect_identical(dir(tmp), basename(path))
  expect_equal(readLines(path), readLines(file.path(tmp, basename(path))))

  unlink(path)
  unlink(tmp, recursive = TRUE)
})


test_that("support_set_login", {
  cl <- null_docker_client()
  auth <- cl$.api_client$auth

  expect_null(auth$get("server.io"))
  params <- list(body = jsonlite::toJSON(list(serveraddress = "server.io",
                                              username = "foo",
                                              password = "bar"),
                                         auto_unbox = TRUE))
  after_system_login(NULL, params, cl)
  expect_equal(auth$get("server.io"),
               base64encode(params$body))
})


test_that("after_container_list", {
  data <- data_frame(names = I(list("/foo", "/bar")),
                     other = c("a", "b"))
  res <- after_container_list(data)
  expect_equal(res$name, vcapply(res$names, identity))
  expect_equal(res$name, sub("^/", "", vcapply(data$names, identity)))
})


test_that("after_container_create", {
  response <- list(id = dummy_id())
  cl <- null_docker_client()

  res <- after_container_create(response, NULL, cl)
  expect_is(res, "docker_container")
  expect_equal(res$id(), dummy_id())
})


test_that("after_container_create: warnings", {
  response <- list(id = dummy_id(), warnings = "here is a warning")
  cl <- null_docker_client()
  expect_warning(res <- after_container_create(response, NULL, cl),
                 "here is a warning")
  expect_is(res, "docker_container")
  expect_equal(res$id(), dummy_id())
})


test_that("after_container_archive", {
  bytes <- as.raw(sample(0:255))
  expect_identical(after_container_archive(bytes, list(), NULL),
                   bytes)

  path <- tempfile()
  on.exit(unlink(path))

  expect_identical(after_container_archive(bytes, list(dest = path)), path)
  expect_identical(read_binary(path), bytes)
})


test_that("after_exec_create", {
  cl <- null_docker_client()
  res <- after_exec_create(list(id = dummy_id()), NULL, cl)
  expect_is(res, "docker_exec")
  expect_identical(res$id(), dummy_id())
})


test_that("after_container_logs", {
  bin <- as.raw(0:10)
  expect_identical(after_container_logs("hello", NULL, NULL), "hello")
  expect_identical(after_container_logs(bin, NULL, NULL), bin)

  response <- list(content_handler = rawToChar,
                   response = list(content = charToRaw("hello")))
  params <- list(query = list(follow = TRUE))

  expect_identical(after_container_logs(response, params), "hello")
})


## For now this is identical to the tests for `after_container_logs` -
## the functions could be merged together but that's not essential
## really.
test_that("after_task_logs", {
  bin <- as.raw(0:10)
  expect_identical(after_task_logs("hello", NULL, NULL), "hello")
  expect_identical(after_task_logs(bin, NULL, NULL), bin)

  response <- list(content_handler = rawToChar,
                   response = list(content = charToRaw("hello")))
  params <- list(query = list(follow = TRUE))

  expect_identical(after_task_logs(response, params), "hello")
})


test_that("after_container_path_stat", {
  data <- list(a = 1, b = "hello")
  str <- jsonlite::toJSON(data, auto_unbox = TRUE)
  response <- list(docker_container_path_stat = base64encode(str))
  expect_equal(after_container_path_stat(response, NULL, NULL), data)
})


test_that("after_container_top", {
  d <- read_sample_response("sample_responses/v1.29/container_top.R")
  cmp <- data_frame(
    UID = "root",
    PID = c("13642", "13735"),
    PPID = c("882", "13642"),
    C = "0",
    STIME = c("17:03", "17:06"),
    TTY = "pts/0",
    TIME = "00:00:00",
    CMD = c("/bin/bash", "sleep 10"))

  expect_equal(after_container_top(d$reference, NULL, NULL), cmp)
})


test_that("after_image_commit", {
  cl <- null_docker_client()
  img <- after_image_commit(list(id = dummy_id()), NULL, cl)
  expect_is(img, "docker_image")
  expect_equal(img$id(), dummy_id())
})


test_that("after_image_build", {
  cl <- null_docker_client()

  bin <- read_binary("sample_responses/build/success")
  id <- "8d9538fe3885"

  prev <- set_dummy_id(id)
  on.exit(set_dummy_id(prev))

  response <- list(response = list(content = bin))

  img <- after_image_build(response, NULL, cl)
  expect_is(img, "docker_image")
  expect_equal(img$id(), id)
})


test_that("after_image_pull", {
  cl <- null_docker_client()

  params <- list(query = list(fromImage = "ubuntu", tag = "latest"))
  id <- sprintf("%s:%s", params$query$fromImage, params$query$tag)

  prev <- set_dummy_id(id)
  on.exit(set_dummy_id(prev))

  img <- after_image_pull(NULL, params, cl)
  expect_is(img, "docker_image")
  expect_equal(img$id(), id)
})


test_that("after_image_push", {
  f <- function(x) {
    list(response =
           list(content = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE))))
  }

  expect_equal(withVisible(after_image_push(f(list(status = TRUE)))),
               list(value = TRUE, visible = FALSE))
  e <- get_error(after_image_push(f(list(error = "my error"))))
  expect_is(e, "push_error")
  expect_is(e, "error")
  expect_is(e, "condition")
  expect_equal(e$message, "my error")
})


test_that("after_network_create", {
  cl <- null_docker_client()
  res <- after_network_create(list(id = dummy_id()), NULL, cl)
  expect_is(res, "docker_network")
  expect_equal(res$id(), dummy_id())
})


test_that("after_volume_create", {
  cl <- null_docker_client()
  res <- after_volume_create(list(name = dummy_id()), NULL, cl)
  expect_is(res, "docker_volume")
  expect_equal(res$name(), dummy_id())
})


test_that("after_volume_list", {
  response <- list(volumes = mtcars)
  expect_identical(after_volume_list(response), response$volumes)

  response$warnings <- "something happened"
  expect_warning(res <- after_volume_list(response),
                  response$warnings)
  expect_equal(res, response$volumes)
})


test_that("after_exec_start", {
  str <- "this is a string"
  response <- list(response = list(content = charToRaw(str)))
  res <- withVisible(after_exec_start(response))
  expect_identical(res, list(value = str, visible = FALSE))
})


test_that("after_container_update", {
  cl <- null_docker_client()
  self <- docker_client_container(dummy_id(), cl)

  response <- NULL
  expect_identical(after_container_update(response, NULL, self), self)

  response <- list(warnings = "this is a warning")
  expect_warning(res <- after_container_update(response, NULL, self),
                 "this is a warning")
  expect_identical(res, self)
})


test_that("invisible_self", {
  r <- runif(20)
  expect_identical(invisible_self(NULL, NULL, r), r)
  expect_silent(invisible_self(stop("untouched"), stop("untouched"), r))
})


test_that("docker_client_container_image", {
  cl <- null_docker_client()
  self <- docker_client_container(dummy_id(), cl)

  id <- random_hex(32)
  prev <- set_dummy_id(id)
  on.exit(set_dummy_id(prev))

  update_dummy_attrs(self, list(image = paste0("sha256:", id)))

  img <- docker_client_container_image(self)
  expect_is(img, "docker_image")
  expect_equal(img$id(), id)
})


test_that("docker_client_image_tags", {
  f <- function(...) {
    list(repo_tags = c(...))
  }

  expect_equal(docker_client_image_tags(f(character(0))), character(0))
  expect_equal(docker_client_image_tags(f("foo:bar")), "foo:bar")
  expect_equal(docker_client_image_tags(f("<none>:<none>", "foo:bar")),
               "foo:bar")

  cl <- null_docker_client()
  image <- docker_client_image(dummy_id(), cl)

  update_dummy_attrs(image, f(character(0)))
  expect_equal(image$tags(FALSE), character(0))

  update_dummy_attrs(image, f("<none>:<none>", "foo:bar"))
  expect_equal(image$tags(FALSE), "foo:bar")
})


test_that("docker_client_network_containers", {
  cl <- null_docker_client()
  nw <- docker_client_network(dummy_id(), cl)
  expect_identical(nw$containers(), list())

  id <- random_hex(32)
  prev <- set_dummy_id(id)
  on.exit(set_dummy_id(prev))

  update_dummy_attrs(nw, list(containers = set_names(list(NULL), id)))

  res <- nw$containers(FALSE)
  expect_is(res, "list")
  expect_equal(length(res), 1L)
  expect_is(res[[1]], "docker_container")
  expect_identical(res[[1]]$id(), id)
})


test_that("docker_client_volume_map", {
  attrs <- list(name = "foo")
  expect_equal(docker_client_volume_map(attrs, "dest", FALSE),
               "foo:dest")
  expect_equal(docker_client_volume_map(attrs, "dest", TRUE),
               "foo:dest:ro")

  id <- "myvolume"
  prev <- set_dummy_id(id)
  on.exit(set_dummy_id(prev))

  cl <- null_docker_client()
  vol <- docker_client_volume(dummy_id(), cl)

  expect_equal(vol$map("/dest"), "myvolume:/dest")
  expect_equal(vol$map("/dest", TRUE), "myvolume:/dest:ro")
})


test_that("after_service_create", {
  cl <- null_docker_client()
  res <- after_service_create(list(id = dummy_id()), list(detach = TRUE), cl)
  expect_is(res, "docker_service")
  expect_equal(res$id(), dummy_id())
})


test_that("after_secret_create", {
  expect_equal(after_secret_create(list(id = 1)), 1)
})


test_that("after_secret_list", {
  id <- replicate(2, rand_str())
  name <- replicate(2, rand_str())
  other <- runif(2)
  spec <- lapply(name, function(x) list(name = x, value = runif(1)))
  d <- data.frame(id = replicate(2, rand_str()),
                  other = other,
                  spec = I(spec))

  d2 <- after_secret_list(d)
  expect_equal(names(d2), c("id", "name", "other", "spec"))
  expect_equal(d2$name, name)

  d3 <- after_secret_list(d[integer(0), ])
  expect_equal(d3, d2[integer(0), ])
})


test_that("validate_secret_data", {
  expect_equal(validate_secret_data("hello"),
               base64encode("hello", TRUE))

  bytes <- serialize(iris, NULL)
  expect_equal(validate_secret_data(bytes),
               base64encode(bytes, TRUE))

  expect_error(validate_secret_data(1),
               "'data' must be a scalar character or raw", fixed = TRUE)
})


test_that("validate_service_secrets", {
  cl <- function(id, name) {
    secrets <- data.frame(id = id, name = name, stringsAsFactors = FALSE)
    list(secrets = list(list = function() secrets))
  }

  f <- function(secrets) {
    cl <- null_docker_client()
    cl$types$task_spec(
      cl$types$container_spec(
        image = "foo",
        secrets = secrets))
  }

  expect_equal(validate_service_secrets(f(NULL), NULL), f(NULL))

  expect_error(validate_service_secrets(f("foo"), cl("a", "b")),
               "Unknown secret: 'foo'")
  expect_error(validate_service_secrets(f(c("foo", "bar")), cl("a", "b")),
               "Unknown secrets: 'foo', 'bar'")

  u <- jsonlite::unbox
  s <-
    validate_service_secrets(f("foo"), cl("foo", "bar"))$ContainerSpec$Secrets
  cmp <- list(list(SecretID = u("foo"), SecretName = u("bar"),
                   File = list(Name = u("bar"), UID = u("0"), GID = u("0"),
                               Mode = u(292L))))
  expect_equal(s, cmp)

  u <- jsonlite::unbox
  s <-
    validate_service_secrets(f("bar"), cl("foo", "bar"))$ContainerSpec$Secrets
  expect_equal(s, cmp)
})


test_that("validate_service_configs", {
  cl <- function(id, name) {
    configs <- data.frame(id = id, name = name, stringsAsFactors = FALSE)
    list(configs = list(list = function() configs))
  }

  f <- function(configs) {
    cl <- null_docker_client(api_version = "1.30")
    cl$types$task_spec(
      cl$types$container_spec(
        image = "foo",
        configs = configs))
  }

  expect_equal(validate_service_configs(f(NULL), NULL), f(NULL))

  expect_error(validate_service_configs(f("foo"), cl("a", "b")),
               "Unknown config: 'foo'")
  expect_error(validate_service_configs(f(c("foo", "bar")), cl("a", "b")),
               "Unknown configs: 'foo', 'bar'")

  u <- jsonlite::unbox
  s <-
    validate_service_configs(f("foo"), cl("foo", "bar"))$ContainerSpec$Configs
  cmp <- list(list(ConfigID = u("foo"), ConfigName = u("bar"),
                   File = list(Name = u("bar"), UID = u("0"), GID = u("0"),
                               Mode = u(292L))))
  expect_equal(s, cmp)

  u <- jsonlite::unbox
  s <-
    validate_service_configs(f("bar"), cl("foo", "bar"))$ContainerSpec$Configs
  expect_equal(s, cmp)
})


test_that("validate_service_replicas", {
  expect_null(validate_service_replicas(NULL, FALSE))

  expect_equal(
    validate_service_replicas(2L, FALSE),
    list(Replicated = list(Replicas = jsonlite::unbox(2L))))
  expect_error(
    validate_service_replicas(pi, FALSE),
    "'replicas' must be a scalar integer (non-NA)",
    fixed = TRUE)

  expect_equal(
    validate_service_replicas(NULL, TRUE),
    list(Global = NULL))
  expect_error(
    validate_service_replicas(pi, TRUE),
    "Cannot use 'replicas' with 'global'",
    fixed = TRUE)
})


test_that("docker_client_service_tasks (offline)", {
  expect_error(docker_client_service_tasks(NULL, list(service = "foo")),
               "'service' is not a valid filter name for this method")
})


test_that("service progress: null", {
  pr <- make_service_start_progress(NULL)
  expect_is(pr, "function")
  expect_silent(ans <- pr("running"))
  expect_null(ans)
})


test_that("service progress", {
  txt <- capture.output({
    pr <- make_service_start_progress(stdout())
    pr(c("assigned", "ready"))
  })
  expect_equal(
    txt,
    c("new > alloc > pend > assign > accept > prep > ready > start > running",
        "=========================>1---------------------->1"))

  txt <- capture.output({
    pr <- make_service_start_progress(stdout())
    pr("new")
    pr(rep("new", 10))
    pr(c("assigned", "ready", rep("preparing", 12)))
  })
  expect_equal(
    txt,
    c("new > alloc > pend > assign > accept > prep > ready > start > running",
      "=>1",
      ">10",
      "=========================>1------------->12------>1"))
})


test_that("validate_plugin_privileges", {
  dat <- data_frame(
    name = c("network", "mount", "mount", "device", "capabilities"),
    description = c(
      "permissions to access a network", "host path to mount",
      "host path to mount",
      "host device to access", "list of additional capabilities required"),
    value = I(list("host", "/var/lib/docker/plugins/", "", "/dev/fuse",
                   "CAP_SYS_ADMIN")))

  f <- function(i) {
    list(Name = jsonlite::unbox(dat$name[[i]]),
         Description = jsonlite::unbox(dat$description[[i]]),
         Value = dat$value[[i]])
  }
  cmp <- as.character(jsonlite::toJSON(lapply(seq_len(nrow(dat)), f)))

  expect_message(
    res <- validate_plugin_privileges(NULL, "vieux/sshfs:latest", TRUE, dat),
    "Plugin 'vieux/sshfs:latest' is requesting permissions:",
    fixed = TRUE)
  expect_message(
    res <- validate_plugin_privileges(NULL, "vieux/sshfs:latest", TRUE, dat),
    "  - host path to mount (mount): [/var/lib/docker/plugins/]",
    fixed = TRUE)
  expect_equal(res, cmp)

  expect_error(
    validate_plugin_privileges(NULL, "vieux/sshfs:latest", FALSE, dat),
    "Not installing plugin 'vieux/sshfs:latest'")
})


test_that("after_plugin_install", {
  cl <- null_docker_client()

  p <- list(query = list(name = dummy_id(), remote = "xxx"),
            disable = TRUE)
  res <- after_plugin_install(NULL, p, cl$plugins)
  expect_is(res, "docker_plugin")
  expect_equal(res$name(), dummy_id())

  p <- list(query = list(name = NULL, remote = dummy_id()),
            disable = TRUE)

  res <- after_plugin_install(NULL, p, cl$plugins)
  expect_is(res, "docker_plugin")
  expect_equal(res$name(), dummy_id())

  p <- list(query = list(name = NULL, remote = dummy_id()),
            disable = FALSE)
  expect_error(after_plugin_install(NULL, p, cl$plugins),
               "Can't make requests with the null client")
})


test_that("validate_plugin_configure_body", {
  expect_equal(validate_plugin_configure_body(NULL), "{}")
  expect_equal(
    validate_plugin_configure_body(c(foo = "xxx")),
    as.character(jsonlite::toJSON(list(foo = "xxx"), auto_unbox = TRUE)))
})
