context("docker client: images")

test_that("list", {
  d <- test_docker_client()$image$list()
  expect_is(d, "data.frame")
})

test_that("get", {
  img <- test_docker_client()$image$get("hello-world")
  expect_is(img, "docker_image")
  expect_is(img, "stevedore_object")
  expect_equal(img$name(), "hello-world")

  expect_true("hello-world:latest" %in% img$tags())
})

test_that("get by reference", {
  sha <- "sha256:f2a9173236"
  cl <- test_docker_client()
  img1 <- cl$image$get("hello-world:latest")

  sha <- img1$short_id()
  img2 <- test_docker_client()$image$get(sha)
  expect_true("hello-world:latest" %in% img1$tags())
  expect_equal(img2$name(), sha)
  expect_equal(img2$id(), img1$id())
})

test_that("id/short_id", {
  img <- test_docker_client()$image$get("hello-world")
  d <- test_docker_client()$image$list()
  i1 <- img$id()
  i2 <- img$short_id()
  expect_true(i1 %in% d$id)
  expect_true(string_starts_with(i1, i2))
  expect_equal(nchar(i1), 71L)
  expect_equal(nchar(i2), 17L)
})

test_that("inspect", {
  img <- test_docker_client()$image$get("hello-world")
  d <- img$inspect()
  expect_is(d, "list")
})

test_that("tag/reload/untag", {
  img <- test_docker_client()$image$get("hello-world")
  tag <- rand_str(10)
  prev <- img$tags()
  expect <- paste0(tag, ":latest")
  res <- img$tag(tag)
  expect_identical(res, img)
  expect_true(expect %in% img$tags())
  res <- img$untag(expect)
  expect_false(expect %in% img$tags())
})

test_that("untag - invalid tag", {
  img <- test_docker_client()$image$get("hello-world")
  tag <- rand_str(10)
  expect_error(img$untag(tag), sprintf("Invalid repo_tag '%s:latest'", tag))
})

test_that("history", {
  img <- test_docker_client()$image$get("hello-world")
  h <- img$history()
  expect_is(h, "data.frame")
  expect_equal(h$id[[1]], img$id())
})

test_that("export", {
  img <- test_docker_client()$image$get("hello-world")
  tar <- img$export()
  expect_is(tar, "raw")

  path <- tempfile_test()
  writeBin(tar, path)

  extract <- tempfile_test()
  dir.create(extract)
  untar(path, exdir = extract)
  expect_true(file.exists(file.path(extract, "manifest.json")))
})

test_that("import", {
  cl <- test_docker_client()
  img <- cl$image$get("hello-world")
  id <- img$id()
  tar <- img$export()

  img$remove(force = TRUE)

  ## TODO: need to make the errors here a bit easier to work with
  ## programmatically.  At least they're captured for now!
  e <- get_error(cl$image$get("hello-world"))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)

  d <- cl$image$import(tar)
  img2 <- cl$image$get(id)
  img2$tag("hello-world")

  img3 <- cl$image$get("hello-world")

  expect_equal(img2$export(), tar)
  expect_equal(img3$export(), tar)
})


test_that("build: success", {
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  txt <- capture.output({
    ans <- cl$image$build(context, nocache = TRUE, rm = TRUE,
                          tag = "richfitz/iterate:testing")})

  expect_match(txt, "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})


test_that("build: multitag", {
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  nm <- rand_str(8, "stevedore_")
  tag <- sprintf("%s:%s", nm, c("foo", "bar"))

  ans <- cl$image$build(context, nocache = TRUE, rm = TRUE,
                        tag = tag, stream = FALSE)
  expect_true(setequal(ans$tags(), tag))
})


test_that("build: stream output", {
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  path <- tempfile_test()
  con <- file(path, "wb")
  on.exit(close(con))

  expect_silent(
    ans <- cl$image$build(context, nocache = TRUE, rm = TRUE, stream = con,
                          tag = "richfitz/iterate:testing"))
  close(con)
  on.exit()

  txt <- readLines(path)
  expect_match(txt, "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: stream output with file arg", {
  path <- tempfile_test()
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  expect_silent(
    ans <- cl$image$build(context, nocache = TRUE, rm = TRUE, stream = path,
                          tag = "richfitz/iterate:testing"))
  expect_true(file.exists(path))
  expect_match(readLines(path), "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: context as directory name", {
  path <- tempfile_test()
  cl <- test_docker_client()
  expect_silent(
    ans <- cl$image$build("images/iterate", nocache = TRUE,
                          rm = TRUE, stream = path,
                          tag = "richfitz/iterate:testing"))
  expect_match(readLines(path), "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: failure", {
  cl <- test_docker_client()
  ## As above, but missing a resource:
  path <- tempfile_test()
  dir.create(path)
  file.copy("images/iterate/Dockerfile", path)
  context <- tar_directory(path)

  txt <- capture.output({
    ans <- get_error(cl$image$build(context, nocache = TRUE, rm = TRUE,
                                    tag = "richfitz/iterate:failure"))})
  expect_is(ans, "build_error")
})


test_that("build: dockerignore", {
  cl <- test_docker_client()

  path <- tempfile_test()
  dir.create(path)
  file.copy("images/iterate", path, recursive = TRUE)

  paths <- c(paste0("dir1/", c("a.txt", "b.md", "c.c")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             paste0("dir3/", c("bar.md", "bar.c")),
             "README.md")
  root <- make_fake_files(paths)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dockerfile <- c("FROM alpine:latest",
                  "COPY . /contents",
                  "WORKDIR /contents")
  writeLines(dockerfile, file.path(root, "Dockerfile"))

  list_files <- function(container) {
    logs <- cl$container$run(container, c("find", "."),
                             rm = TRUE, stream = FALSE)$logs
    setdiff(sub("^\\./", "", trimws(as.vector(logs))), ".")
  }

  res1 <- cl$image$build(root, stream = FALSE)
  files1 <- list_files(res1)

  writeLines("**/*.md", file.path(root, ".dockerignore"))

  res2 <- cl$image$build(root, stream = FALSE)
  files2 <- list_files(res2)

  expect_equal(sort(files2),
               sort(setdiff(c(".dockerignore", files1),
                            grep("\\.md$", files1, value = TRUE))))
})


test_that("prune", {
  cl <- test_docker_client()
  cl$image$prune()

  ans <- cl$image$build("images/iterate", nocache = TRUE, rm = TRUE,
                        stream = FALSE, tag = "richfitz/iterate:testing")
  cl$image$remove("richfitz/iterate:testing", noprune = TRUE)

  res <- cl$image$prune()
  expect_is(res$images_deleted, "data.frame")
  expect_true(nrow(res$images_deleted) > 0L)
  expect_true(all(is.na(res$images_deleted$untagged)))
  expect_false(any(is.na(res$images_deleted$deleted)))

  res <- cl$image$prune()
  expect_is(res$images_deleted, "data.frame")
  expect_equal(nrow(res$images_deleted), 0L)
  expect_equal(res$space_reclaimed, 0L)
})


test_that("pull", {
  skip_if_no_internet()

  cl <- test_docker_client()
  try(cl$image$remove("alpine:3.1"), silent = TRUE)
  txt <- capture.output(img <- cl$image$pull("alpine:3.1"))

  expect_true("alpine:3.1" %in% img$tags())
  expect_match(txt, "Downloaded newer", all = FALSE)
})

test_that("push", {
  skip("not yet tested")
})

test_that("search", {
  skip_if_no_internet()
  cl <- test_docker_client()
  ans <- cl$image$search("modeladequacy", limit = 10L)
  expect_is(ans, "data.frame")
  expect_match(ans$name, "richfitz/modeladequacy", all = FALSE)
  i <- ans$name == "richfitz/modeladequacy"
  expect_false(ans$is_official[i])
  expect_false(ans$is_automated[i])
})

## This tests that we have everything plumbed enough that we do see
## api differences in the responses when requesting different api
## versions.
test_that("api versions", {
  d1 <- test_docker_client(api_version = "1.29")
  d2 <- test_docker_client(api_version = "1.26")

  info_1 <- d1$image$get("hello-world")$inspect()
  info_2 <- d2$image$get("hello-world")$inspect()

  expect_true("os_version" %in% names(info_1))
  expect_false("os_version" %in% names(info_2))
})

test_that("export", {
  cl <- test_docker_client()
  expect_error(cl$image$export(character()),
               "'names' must be a character vector (non zero length, non-NA)",
               fixed = TRUE)
  b1 <- cl$image$export("hello-world")
  b2 <- cl$image$export("alpine")
  b3 <- cl$image$export(c("hello-world", "alpine"))

  expect_is(b1, "raw")
  expect_is(b2, "raw")
  expect_is(b3, "raw")

  expect_true(length(b3) > length(b1))
  expect_true(length(b3) > length(b2))
})

test_that("push/pull with auth", {
  skip_if_no_internet()

  cl <- test_docker_client()

  img <- cl$image$get("richfitz/iterate:latest")
  tag <- "stevedorebot/secret:latest"
  img$tag(tag)
  img$reload()
  on.exit(img$untag(tag))

  cl <- test_docker_client()

  err <- get_error(cl$image$pull(tag, stream = FALSE))
  expect_is(err, "docker_error")
  expect_equal(err$code, 404L)

  err <- get_error(cl$image$push(tag, stream = FALSE))
  expect_is(err, "push_error")
  expect_null(err$code)

  pw <- get_stevedorebot_pass()
  cl$login("stevedorebot", pw, serveraddress = "docker.io")

  expect_true(cl$image$push(tag, stream = FALSE))
  img2 <- cl$image$pull(tag, stream = FALSE)
  expect_is(img2, "docker_image")
  expect_equal(img2$id(), img$id())
})


test_that("get (offline)", {
  cl <- null_docker_client()
  x <- cl$image$get(dummy_id())
  expect_is(x, "docker_image")
  expect_equal(x$id(), dummy_id())
})
