context("docker client: images")

test_that("list", {
  d <- test_docker_client()$images$list()
  expect_is(d, "data.frame")
})

test_that("get", {
  img <- test_docker_client()$images$get("hello-world")
  expect_is(img, "docker_image")
  expect_is(img, "stevedore_object")
  expect_equal(img$name(), "hello-world")

  expect_true("hello-world:latest" %in% img$tags())
})

test_that("get by reference", {
  sha <- "sha256:f2a9173236"
  img <- test_docker_client()$images$get(sha)
  expect_true("hello-world:latest" %in% img$tags())
  expect_equal(img$name(), sha)
})

test_that("id/short_id", {
  img <- test_docker_client()$images$get("hello-world")
  d <- test_docker_client()$images$list()
  i1 <- img$id()
  i2 <- img$short_id()
  expect_true(i1 %in% d$id)
  expect_true(string_starts_with(i1, i2))
  expect_equal(nchar(i1), 71L)
  expect_equal(nchar(i2), 17L)
})

test_that("inspect", {
  img <- test_docker_client()$images$get("hello-world")
  d <- img$inspect()
  expect_is(d, "list")
})

test_that("tag/reload/untag", {
  img <- test_docker_client()$images$get("hello-world")
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
  img <- test_docker_client()$images$get("hello-world")
  tag <- rand_str(10)
  expect_error(img$untag(tag), sprintf("Invalid repo_tag '%s:latest'", tag))
})

test_that("history", {
  img <- test_docker_client()$images$get("hello-world")
  h <- img$history()
  expect_is(h, "data.frame")
  expect_equal(h$id[[1]], img$id())
})

test_that("export", {
  img <- test_docker_client()$images$get("hello-world")
  tar <- img$export()
  expect_is(tar, "raw")

  path <- tempfile()
  writeBin(tar, path)

  extract <- tempfile()
  dir.create(extract)
  untar(path, exdir = extract)
  expect_true(file.exists(file.path(extract, "manifest.json")))
})

test_that("import", {
  cl <- test_docker_client()
  img <- cl$images$get("hello-world")
  id <- img$id()
  tar <- img$export()

  img$remove(force = TRUE)

  ## TODO: need to make the errors here a bit easier to work with
  ## programmatically.  At least they're captured for now!
  e <- get_error(cl$images$get("hello-world"))
  expect_is(e, "docker_error")
  expect_equal(e$code, 404L)

  d <- cl$images$import(tar)
  img2 <- cl$images$get(id)
  img2$tag("hello-world")

  img3 <- cl$images$get("hello-world")

  expect_equal(img2$export(), tar)
  expect_equal(img3$export(), tar)
})

test_that("prune", {
  skip("Not yet testable")
})

test_that("build: success", {
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  txt <- capture.output({
    ans <- cl$images$build(context, nocache = TRUE, rm = TRUE,
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

  ans <- cl$images$build(context, nocache = TRUE, rm = TRUE,
                         tag = tag, stream = FALSE)
  expect_true(setequal(ans$tags(), tag))
})


test_that("build: stream output", {
  path <- tempfile()
  con <- file(path, "wb")
  on.exit(close(con))
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  expect_silent(
    ans <- cl$images$build(context, nocache = TRUE, rm = TRUE, stream = con,
                           tag = "richfitz/iterate:testing"))
  close(con)
  on.exit()

  txt <- readLines(path)
  expect_match(txt, "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: stream output with file arg", {
  path <- tempfile()
  cl <- test_docker_client()
  context <- tar_directory("images/iterate")

  expect_silent(
    ans <- cl$images$build(context, nocache = TRUE, rm = TRUE, stream = path,
                           tag = "richfitz/iterate:testing"))
  expect_true(file.exists(path))
  expect_match(readLines(path), "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: context as directory name", {
  path <- tempfile()
  cl <- test_docker_client()
  expect_silent(
    ans <- cl$images$build("images/iterate", nocache = TRUE,
                           rm = TRUE, stream = path,
                           tag = "richfitz/iterate:testing"))
  expect_match(readLines(path), "Successfully built", all = FALSE)
  expect_is(ans, "docker_image")
  expect_equal(ans$tags(), "richfitz/iterate:testing")
})

test_that("build: failure", {
  cl <- test_docker_client()
  ## As above, but missing a resource:
  path <- tempfile()
  dir.create(path)
  file.copy("images/iterate/Dockerfile", path)
  context <- tar_directory(path)

  txt <- capture.output({
    ans <- get_error(cl$images$build(context, nocache = TRUE, rm = TRUE,
                                     tag = "richfitz/iterate:failure"))})
  expect_is(ans, "build_error")
})


test_that("build: dockerignore", {
  cl <- test_docker_client()

  path <- tempfile()
  dir.create(path)
  file.copy("images/iterate", path, recursive = TRUE)

  paths <- c(paste0("dir1/", c("a.txt", "b.md", "c.c")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             paste0("dir3/", c("bar.md", "bar.c")),
             "README.md")
  root <- make_fake_files(paths)
  dockerfile <- c("FROM alpine:latest",
                  "COPY . /contents",
                  "WORKDIR /contents")
  writeLines(dockerfile, file.path(root, "Dockerfile"))

  list_files <- function(container) {
    logs <- cl$containers$run(container, c("find", "."),
                              rm = TRUE, stream = FALSE)$logs
    setdiff(sub("^\\./", "", trimws(as.vector(logs))), ".")
  }

  res1 <- cl$images$build(root)
  files1 <- list_files(res1)

  writeLines("**/*.md", file.path(root, ".dockerignore"))

  res2 <- cl$images$build(root)
  files2 <- list_files(res2)

  expect_equal(sort(files2),
               sort(setdiff(c(".dockerignore", files1),
                            grep("\\.md$", files1, value = TRUE))))
})

test_that("pull", {
  skip_if_no_internet()

  cl <- test_docker_client()
  try(cl$images$remove("alpine:3.1"), silent = TRUE)
  txt <- capture.output(img <- cl$images$pull("alpine:3.1"))

  expect_true("alpine:3.1" %in% img$tags())
  expect_match(txt, "Downloaded newer", all = FALSE)
})

test_that("push", {
  skip("not yet tested")
})

test_that("search", {
  skip_if_no_internet()
  cl <- test_docker_client()
  ans <- cl$images$search("modeladequacy", limit = 10L)
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

  info_1 <- d1$images$get("hello-world")$inspect()
  info_2 <- d2$images$get("hello-world")$inspect()

  expect_true("os_version" %in% names(info_1))
  expect_false("os_version" %in% names(info_2))
})

test_that("export", {
  cl <- test_docker_client()
  expect_error(cl$images$export(character()),
               "'names' must be a character vector (non zero length, non-NA)",
               fixed = TRUE)
  b1 <- cl$images$export("hello-world")
  b2 <- cl$images$export("alpine")
  b3 <- cl$images$export(c("hello-world", "alpine"))

  expect_is(b1, "raw")
  expect_is(b2, "raw")
  expect_is(b3, "raw")

  expect_true(length(b3) > length(b1))
  expect_true(length(b3) > length(b2))
})
