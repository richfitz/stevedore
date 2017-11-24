context("docker client: images")

test_that("list", {
  d <- test_docker_client()$images$list()
  expect_is(d, "data.frame")
})

test_that("get", {
  img <- test_docker_client()$images$get("hello-world")
  expect_is(img, "docker_image")
  expect_is(img, "stevedore_object")

  expect_true("hello-world:latest" %in% img$tags())
})

test_that("get by reference", {
  img <- test_docker_client()$images$get("sha256:f2a9173236")
  expect_true("hello-world:latest" %in% img$tags())
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
  expect <- paste0(tag, ":latest")
  img$tag(tag)
  expect_false(expect %in% img$tags())
  tmp <- img$reload()
  expect_identical(tmp, img)
  expect_true(expect %in% img$tags())
  expect_equal(img$untag(expect),
               data.frame(untagged = expect, deleted = NA_character_,
                          stringsAsFactors = FALSE))
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

test_that("build", {
  skip("not yet tested")
})

test_that("pull", {
  skip("not yet implemented")
})

test_that("push", {
  skip("not yet tested")
})

test_that("search", {
  skip("not yet tested")
})
