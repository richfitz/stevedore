context("docker client: containers")

test_that("create", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  expect_is(x, "docker_container")
  expect_is(x, "stevedore_object")

  expect_equal(x$name(), nm)
  expect_equal(x$id(), x$inspect()$id)
  expect_equal(x$inspect()$name, paste0("/", nm))
  expect_identical(x$reload(), x)

  expect_null(x$remove())

  e <- get_error(x$inspect())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
  expect_equal(e$endpoint, "container_inspect")
  expect_equal(e$reason, "no such container")
})

test_that("create, using image", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  img <- d$images$get("hello-world")
  x <- d$containers$create(img, name = nm)
  expect_equal(x$image()$id(), img$id())
  x$remove()
})

test_that("get", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x1 <- d$containers$create("hello-world", name = nm)
  x2 <- d$containers$get(nm)
  expect_identical(x1$inspect(FALSE), x2$inspect(FALSE))

  d$containers$remove(nm)

  e <- get_error(d$containers$get(nm))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("list", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  cl <- d$containers$list(all = TRUE)
  expect_is(cl, "data.frame")
  expect_true("names" %in% names(cl))
  expect_true(list(nm) %in% cl$names)
  expect_true(nm %in% cl$name)

  x$remove()
})

test_that("list with arguments", {
  d <- test_docker_client()
  cl <- d$containers$list()

  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  ## First, the easy arguments:
  cl <- d$containers$list(all = TRUE, limit = 10L, size = TRUE)
  i <- match(nm, cl$name)
  expect_false(is.na(i))
  expect_false(is.na(cl$size_root_fs[i]))

  ## Then the filters
  f1 <- c(status = "created")
  f2 <- list(status = "exited")

  cl1 <- d$containers$list(all = TRUE, limit = 10L, size = TRUE, filters = f1)
  cl2 <- d$containers$list(all = TRUE, limit = 10L, size = TRUE, filters = f2)
  expect_true(nm %in% cl1$name)
  expect_false(nm %in% cl2$name)

  expect_identical(x$start(), x)
  x$wait()

  cl1 <- d$containers$list(all = TRUE, limit = 10L, size = TRUE, filters = f1)
  cl2 <- d$containers$list(all = TRUE, limit = 10L, size = TRUE, filters = f2)
  expect_false(nm %in% cl1$name)
  expect_true(nm %in% cl2$name)

  x$remove()
})

test_that("prune", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  ans <- d$containers$prune()
  expect_true(x$id() %in% ans$containers_deleted)
})

test_that("diff", {
  ## TODO: need to get a better test in here - one that actually shows
  ## we can report this.  For now it should be ok to show that the
  ## endpoint works, I guess.
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  xd <- x$diff()

  expect_is(xd, "data.frame")
  x$remove()
})

test_that("export", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  ## TODO: here, and other export type ones - we need some optional
  ## 'filename' argument for dumping the export into.  It might be
  ## worth a small wrapper around it (as a 'tar' class) so that we can
  ## interact more sensibly.  We might also need to stream to disk -
  ## not memory via modifying the request.  This would be done the
  ## same way as 'hijack' is done - but we'd pass through the required
  ## filename and go via curl::curl_fetch_disk rather than
  ## curl_fetch_memory.

  ## TODO: what does one _do_ with an exported container?
  tar <- x$export()
  expect_is(tar, "raw")
  path <- untar_bin(tar)
  expect_true("hello" %in% dir(path))
  unlink(path, recursive = TRUE)
  x$remove()
})

test_that("path_stat", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  ## There's no spec here so not much more we can do
  dat <- x$path_stat("/")
  expect_is(dat, "list")
  expect_equal(dat$name, "/")

  e <- get_error(x$path_stat("foo"))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("archive export", {
  ## TODO: another tar endpoint
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  bin <- x$get_archive("hello", NULL)
  p <- untar_bin(bin)
  expect_true(file.exists(file.path(p, "hello")))

  path <- x$get_archive("hello", tempfile())
  expect_true(file.exists(path))
  expect_identical(readBin(path, raw(), file.size(path)), bin)

  expect_error(x$get_archive("hello", FALSE),
               "'dest' must be a character")
})

test_that("archive import", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)

  e <- get_error(x$path_stat("foo"))
  expect_equal(e$code, 404L)

  path <- tempfile()
  dir.create(path)
  dat <- rand_str(100)
  writeLines(dat, file.path(path, "foo"))

  bin <- tar_directory(path)

  ## Down here, we need to get the types correct for handling.
  x$put_archive(bin, "/")
  bin2 <- x$get_archive("foo", NULL)
  p <- untar_bin(bin2)
  expect_true(file.exists(file.path(p, "foo")))
  expect_identical(readLines(file.path(p, "foo")), dat)
})

test_that("kill", {
  ## NOTE: This test takes a bit longer to run - the start takes ~0.3s
  ## for me.  The timings are about the same on the command line
  ## though, so I'm fairly confident that the issue here is docker and
  ## the speed at which it is able to get the containers up.  It will
  ## become necessary to separate out the tests into "short" and
  ## "long" tests so that I can keep the test cycle reasonable
  ## locally.
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("alpine", cmd = c("sleep", "10000"), name = nm)
  expect_null(x$start())
  expect_equal(x$status(), "running")
  expect_null(x$kill())
  expect_equal(x$status(), "exited")
  x$remove()
})

test_that("logs", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  ## TODO: this only prints out one per second - I'd like to up this
  ## to every 0.1s so that we can run this for less time.
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  expect_null(x$start())
  logs <- x$logs()
  expect_is(logs, "docker_stream")
  expect_equal(logs[[1]], "Reticulating spline 1...\n")
  x$kill()
  x$remove()

  ## TODO: this can be tested separately from the docker bits too (and
  ## should be)
  expect_match(format(logs, style = "plain"),
               "^Reticulating spline \\d+...\n$")
  expect_match(format(logs, style = "prefix"),
               "^O> Reticulating spline \\d+...\n")
  expect_equal(all(crayon::has_style(format(logs, style = "colour"))),
               crayon::has_color())
  expect_match(capture.output(print(logs, style = "plain")),
               "^Reticulating spline \\d+...$")
  expect_match(capture.output(print(logs, style = "prefix")),
               "^O> Reticulating spline \\d+...$")
  expect_match(capture.output(print(logs)), "Reticulating spline \\d+...")
})

test_that("pause/unpause", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  ## TODO: this only prints out one per second - I'd like to up this
  ## to every 0.1s so that we can run this for less time.
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  x$start()
  expect_null(x$pause())
  expect_equal(x$status(), "paused")
  e <- get_error(x$pause())
  expect_is(e, "docker_error")

  ## This is not actually in the spec, but it returns 409 (Conflict)
  ## on pause of paused container - unpausing an unpaused container
  ## returns 500 though so this does not seem reliable.
  ## expect_equal(e$code, 409)
  expect_null(x$unpause())
  expect_equal(x$status(), "running")

  e <- get_error(x$unpause())
  expect_is(e, "docker_error")

  x$kill()
  x$remove()
})

test_that("rename", {
  d <- test_docker_client()
  nm1 <- rand_str(10, "stevedore_")
  nm2 <- rand_str(10, "stevedore_")
  ## TODO: this only prints out one per second - I'd like to up this
  ## to every 0.1s so that we can run this for less time.
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm1)
  expect_null(x$rename(nm2))
  expect_equal(x$reload()$name(), nm2)
  x$remove()
})

test_that("restart", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  x$start()
  Sys.sleep(0.5)
  logs1 <- x$logs()
  expect_null(x$restart(0))
  Sys.sleep(0.5)
  logs2 <- x$logs()

  expect_equal(sum(logs2 == "Reticulating spline 1...\n"), 2)
  x$kill()
  x$remove()
})

test_that("stats", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  s <- x$stats()
  ## There is no spec here so I can't really test much here except
  ## that this does exist.  Note that this is quite slow if using a
  ## running container!
  expect_is(s, "list")
  x$remove()
})

test_that("stop", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  x$start()
  expect_null(x$stop(0))
  expect_equal(x$status(), "exited")
  x$remove()
})

test_that("top", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  x$start()
  t <- x$top()
  expect_is(t, "data.frame")
  expect_true("PID" %in% names(t))
  x$kill()
  x$remove()
})

test_that("update", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("bfirsh/reticulate-splines", name = nm)
  info <- x$inspect(FALSE)
  n <- as.integer(10 * 1001 * 1001)
  y <- x$update(memory = n, memory_swap = -1L)

  expect_identical(x, y)
  info2 <- y$inspect(TRUE)
  expect_identical(info2$host_config$memory, n)
  expect_identical(info2$host_config$memory_swap, -1L)
})

test_that("wait", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  x$start()
  expect_equal(x$wait(), list(status_code = 0L))
  expect_equal(x$status(), "exited")
})

test_that("prune", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  ans <- d$containers$prune()
  expect_true(x$id() %in% ans$containers_deleted)
  e <- get_error(x$status())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
})

test_that("image", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  img <- x$image()
  cmp <- d$images$get("hello-world")
  expect_equal(cmp$inspect(), img$inspect())
  x$remove()
})

test_that("exec", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  ## this sets up a container that will run forever
  x <- d$containers$create("richfitz/iterate",
                           cmd = c("100", "100"),
                           name = nm)
  x$start()
  ans <- x$exec("ls")

  expect_is(ans, "docker_exec")
  expect_is(ans, "stevedore_object")

  info <- ans$inspect()
  expect_false(info$running)
  expect_true(info$open_stdout)
  expect_true(info$open_stderr)

  txt <- capture.output(res <- ans$start(detach = FALSE))
  expect_is(res, "docker_stream")
  expect_equal(txt, strsplit(format(res, style = "prefix"), "\n")[[1]])

  x$kill()
  x$remove()
})

test_that("resize", {
  skip("untested")
})

test_that("attach", {
  skip("attach is not yet implemented")
})

test_that("logs (streaming)", {
  skip("log streaming is not yet implemented")
  ## Do this after build - then we can build a small alpine image that
  ## prints n things over m seconds perhaps
})

test_that("run", {
  d <- test_docker_client()
  ans <- d$containers$run("hello-world")
  expect_equal(names(ans), c("container", "logs"))
  expect_is(ans$container, "docker_container")
  expect_is(ans$logs, "docker_stream")
})

test_that("run: detach", {
  d <- test_docker_client()
  ans <- d$containers$run("richfitz/iterate", c("10", "0.1"), detach = TRUE)
  expect_is(ans, "docker_container")
  expect_equal(ans$wait(), list("status_code" = 0L))
})

test_that("run: no such container", {
  d <- test_docker_client()
  msg <- capture_messages(
    e <- get_error(d$containers$run("richfitz/nosuchcontainer")))
  expect_match(
    msg, "Unable to find image 'richfitz/nosuchcontainer:latest' locally",
    fixed = TRUE, all = FALSE) # allow additional here
  expect_is(e, "docker_error")
  ## TODO: the endpoint registering as failed here is image_create,
  ## not image_pull which is the one that does fail.  This is because
  ## docker_get_image uses tryCatch and *rethrows* the error but what
  ## we should do is withCallingHandlers and then avoid throwing at
  ## all if we can recover.
  if (has_internet()) {
    expect_equal(e$code, 404L)
  } else {
    expect_equal(e$code, 500L)
  }
  expect_equal(e$endpoint, "image_create")
})

test_that("run: remove", {
  d <- test_docker_client()
  ans <- d$containers$run("hello-world", rm = TRUE)
  expect_equal(names(ans), c("container", "logs"))
  expect_is(ans$container, "docker_container")
  expect_is(ans$logs, "docker_stream")
  e <- get_error(d$containers$get(ans$container$id()))
  expect_true(is_docker_error_not_found(e))
})

test_that("run: error", {
  d <- test_docker_client()
  e <- get_error(d$containers$run("richfitz/error", "4", rm = TRUE))

  expect_is(e, "container_error")

  expect_equal(
    e$message,
    "Command '4' in image 'richfitz/error:latest' returned non-zero exit status 4", fixed = TRUE)

  expect_is(e$container, "docker_container")
  expect_equal(e$exit_status, 4L)
  expect_equal(e$cmd, "4")
  expect_equal(e$image$id(), d$images$get("richfitz/error")$id())
  expect_is(e$out, "docker_stream")
  expect_equal(e$out, docker_stream("throwing error 4\n", 1L))
})

test_that("run: error to stderr", {
  d <- test_docker_client()
  e <- get_error(d$containers$run("richfitz/error", "14", rm = TRUE))

  expect_is(e, "container_error")

  expect_equal(
    e$message,
    "Command '14' in image 'richfitz/error:latest' returned non-zero exit status 14\nthrowing error 14 to stderr\n", fixed = TRUE)
})

test_that("run with image", {
  d <- test_docker_client()
  img <- d$images$get("hello-world")
  ans <- d$containers$run(img, rm = TRUE)
  expect_is(ans$logs, "docker_stream")
})

test_that("scalar cmd", {
  d <- test_docker_client()
  c1 <- d$containers$create("alpine:3.1", I("echo hello world"))

  dat <- c1$inspect(FALSE)
  expect_identical(dat$path, "echo")
  expect_identical(dat$args, c("hello", "world"))

  c1$start()
  c1$wait()
  log <- c1$logs()
  expect_equal(format(log, style = "plain", filter = "stdout"),
               "hello world\n")
  c1$remove()
})

test_that("scalar exec", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  ## this sets up a container that will run forever
  x <- d$containers$create("richfitz/iterate",
                           cmd = c("100", "100"),
                           name = nm)
  x$start()
  ans <- x$exec(I("echo hello world"))
  dat <- ans$inspect(FALSE)
  expect_equal(dat$process_config$entrypoint, "echo")
  expect_equal(dat$process_config$arguments, c("hello", "world"))
  capture.output(res <- ans$start(detach = FALSE))

  expect_equal(format(res, style = "plain", filter = "stdout"),
               "hello world\n")
  x$kill()
  x$remove()
})

test_that("stream logs", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  p <- tempfile()
  x <- d$containers$create("richfitz/iterate",
                           cmd = c("10", "0.1"),
                           name = nm)
  x$start()
  res <- x$logs(follow = TRUE, stream = p)
  expect_is(res, "docker_stream")
  cmp <- readLines(p)
  expect_equal(format(res, style = "prefix"), paste0(cmp, "\n"))

  x$remove()
})

test_that("fetch, but don't print, logs", {
  ## Getting some stray beginning of lines using follow on a stopped
  ## container - I *bet* that there's a cat
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("richfitz/iterate",
                           cmd = c("10", "0.1"),
                           name = nm)
  x$start()
  x$wait()
  p <- tempfile()
  expect_silent(v1 <- x$logs(stream = NULL, follow = TRUE))
  expect_silent(v2 <- x$logs(stream = p, follow = TRUE))
  txt <- capture.output(v3 <- x$logs(stream = stdout(), follow = TRUE))

  expect_match(txt, "O> Done!", all = FALSE, fixed = TRUE)
  expect_is(v1, "docker_stream")
  expect_is(v2, "docker_stream")
  expect_is(v3, "docker_stream")

  expect_equal(format(v1, style = "prefix"), paste0(txt, "\n"))
  expect_equal(format(v2, style = "prefix"), paste0(txt, "\n"))
  expect_equal(format(v3, style = "prefix"), paste0(txt, "\n"))

  x$remove()
})

test_that("auto-remove: create", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create(
    "alpine:3.1", cmd = c("echo", "hello world"), name = nm,
    host_config = list(AutoRemove = jsonlite::unbox(TRUE)))
  expect_true(x$inspect(FALSE)$host_config$auto_remove)
  x$start()
  x$wait()
  e <- repeat_until_error(x$inspect)
  expect_equal(e$code, 404L)
})

test_that("auto-remove: run", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$run(
    "alpine:3.1", cmd = c("echo", "hello world"), name = nm,
    rm = TRUE, detach = TRUE)

  expect_true(x$inspect(FALSE)$host_config$auto_remove)
  x$wait()
  e <- repeat_until_error(x$inspect)
  expect_equal(e$code, 404L)
})

## TODO: there is not a test in here (or anywhere really) that this
## shows that this actually streams logs from the beginning (rather
## than just going all at the end).
test_that("stream", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  p <- tempfile()
  x <- d$containers$run("richfitz/iterate",
                        cmd = c("10", "0.1"),
                        name = nm, detach = FALSE,
                        stream = p)
  expect_equal(paste0(readLines(p), "\n"),
               format(x$logs, style = "prefix"))
})

test_that("volume map", {
  ## There's a bit of a trick here - we can't use tempfile() on osx
  ## because the defaults for that path do not include the working
  ## directory.  So we're going to have to use the current directory.
  ## That's dubiously writeable with CRAN's policy though.
  p <- tempfile()
  dir.create(p)
  v <- sprintf("%s:%s", getwd(), "/host")

  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")

  x <- d$containers$create("alpine:latest", cmd = c("ls", "/host"),
                           name = nm, volumes = v)
  x$start()
  x$wait()
  expect_equal(sort(trimws(format(x$logs(), style = "plain"))),
               sort(dir()))
})

test_that("volume map: docker volume", {
  p <- tempfile()
  dir.create(p)

  d <- test_docker_client()
  volume <- d$volumes$create()

  nm <- rand_str(10, "stevedore_")
  v <- sprintf("%s:%s", volume$name(), "/host")

  x <- d$containers$create("richfitz/iterate",
                           cmd = c("100", "100"),
                           name = nm, volumes = v)

  on.exit({
    x$remove(force = TRUE)
    volume$remove()
  })

  x$start()
  e1 <- x$exec(c("touch", "/host/foo"))
  e1$start(detach = FALSE)

  y <- d$containers$create("alpine",
                           cmd = c("ls", "/host"),
                           name = rand_str(10, "stevedore_"), volumes = v)
  y$start()
  y$wait()
  expect_equal(trimws(format(y$logs(), style = "plain")), "foo")
  y$remove()
})

test_that("port map", {
  ## Roughly equivalent to:
  ## docker run --rm -p 10080:80 nginx
  port <- "10080" # port to use - hopefully clear
  ports <- sprintf("%s:80", port)
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("nginx", name = nm, ports = ports)
  x$start()
  on.exit(x$remove(force = TRUE))

  dat <- curl::curl_fetch_memory(sprintf("http://127.0.0.1:%s/", port))
  expect_equal(dat$status_code, 200L)
  expect_true(grepl("nginx", rawToChar(dat$content)))
})

test_that("port map - random free port", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("nginx", name = nm, ports = "80")
  x$start()
  on.exit(x$remove(force = TRUE))

  ports <- x$ports()
  expect_is(ports, "data.frame")
  expect_equal(names(ports),
               c("container_port", "protocol", "host_ip", "host_port"))
  expect_true(all(vlapply(ports, is.character)))
  expect_equal(ports$protocol, "tcp")
  expect_equal(ports$host_ip, "0.0.0.0")
  expect_equal(ports$container_port, "80")

  dat <- curl::curl_fetch_memory(sprintf("http://127.0.0.1:%s/",
                                         ports$host_port))
  expect_equal(dat$status_code, 200L)
  expect_true(grepl("nginx", rawToChar(dat$content)))
})

test_that("query ports of container with none", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("richfitz/iterate",
                           cmd = c("100", "100"),
                           name = nm)
  ports <- x$ports()
  expect_equal(ports, data_frame(container_port = character(0),
                                 protocol = character(0),
                                 host_ip = character(0),
                                 host_port = character(0)))

  x$remove()
})

test_that("network: host", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("nginx", name = nm, ports = "80")
  x$start()
  on.exit(x$remove(force = TRUE), add = TRUE)
  port <- x$ports()$host_port

  ## docker run --rm --network=host richfitz/curl -s http://localhost:32776
  ## docker run --rm richfitz/curl -s http://localhost:32776

  image <- "richfitz/curl"
  args <- c("-s", sprintf("http://localhost:%s", port))

  ## This will fail because we don't have host networking
  y <- d$containers$create(image, args)
  on.exit(y$remove(force = TRUE), add = TRUE)
  y$start()
  code <- y$wait()
  expect_false(code$status_code == 0)
  expect_equal(format(y$logs(), style = "plain"), character(0))

  ## Then with host networking
  z <- d$containers$create(image, args, network = "host")
  on.exit(z$remove(force = TRUE), add = TRUE)
  z$start()
  code <- z$wait()
  expect_true(code$status_code == 0)
  expect_match(format(z$logs(), style = "plain"), "nginx", all = FALSE)
})

test_that("network: custom", {
  ## Here's the scenario:
  ##
  ## docker network create testing
  ## docker run --network=testing --rm -d --name server nginx
  ## docker run --network=testing --rm richfitz/curl -s http://server
  ## docker stop server
  ## docker network rm testing

  server <- rand_str(10, "stevedore_")
  client <- rand_str(10, "stevedore_")
  network <- rand_str(3, "stevedore_")
  url <- paste0("http://", server)

  d <- test_docker_client()
  nw <- d$networks$create(network)
  on.exit({
    if (exists("x")) {
      x$remove(force = TRUE)
    }
    if (exists("y")) {
      y$remove(force = TRUE)
    }
    nw$remove()
  })

  x <- d$containers$create("nginx", name = server, network = nw)
  x$start()

  y <- d$containers$create("richfitz/curl", c("-s", server),
                           network = network, name = client)
  y$start()
  code <- y$wait()
  expect_true(code$status_code == 0)
  expect_match(format(y$logs(), style = "plain"), "nginx", all = FALSE)
})
