context("docker client: containers")

test_that("create", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  expect_is(x, "docker_container")
  expect_is(x, "stevedore_object")

  expect_equal(x$name(), nm)
  expect_equal(x$inspect()$name, paste0("/", nm))
  expect_identical(x$reload(), x)

  expect_null(x$remove())

  e <- get_error(x$inspect())
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)
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

  bin <- x$get_archive("hello")
  p <- untar_bin(bin)
  expect_true(file.exists(file.path(p, "hello")))
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

  bin <- tar_bin(path)

  ## Down here, we need to get the types correct for handling.
  x$put_archive(bin, "/")
  bin2 <- x$get_archive("foo")
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
  expect_equal(x$reload()$status(), "running")
  expect_null(x$kill())
  expect_equal(x$reload()$status(), "exited")
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
  expect_equal(x$reload()$status(), "paused")
  e <- get_error(x$pause())
  expect_is(e, "docker_error")

  ## This is not actually in the spec, but it returns 409 (Conflict)
  ## on pause of paused container - unpausing an unpaused container
  ## returns 500 though so this does not seem reliable.
  ## expect_equal(e$code, 409)
  expect_null(x$unpause())
  expect_equal(x$reload()$status(), "running")

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
  expect_equal(x$reload()$status(), "exited")
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
  expect_equal(x$reload()$status(), "exited")
})

test_that("prune", {
  d <- test_docker_client()
  nm <- rand_str(10, "stevedore_")
  x <- d$containers$create("hello-world", name = nm)
  ans <- d$containers$prune()
  expect_true(x$id() %in% ans$containers_deleted)
  e <- get_error(x$reload()$status())
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

  ## This all looks pretty good really!  But streaming must also be
  ## possible so we need to do some work here in order to get that to
  ## work!
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
  skip("run is not yet implemented")
  ## This is simply a convenience function - and one place I think we
  ## might use dots.
})
