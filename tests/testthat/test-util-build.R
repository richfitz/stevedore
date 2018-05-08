context("dockerignore")


test_that("parse_dockerignore: empty", {
  expect_null(parse_dockerignore(character()))
  expect_null(parse_dockerignore(NULL))
  expect_null(parse_dockerignore("# foo"))
  expect_null(parse_dockerignore("  "))
  expect_null(parse_dockerignore(c("#foo", "  ", "# bar")))
})


test_that("parse_dockerignore: empty", {
  expect_null(parse_dockerignore(character()))
  expect_null(parse_dockerignore(NULL))
  expect_null(parse_dockerignore("# foo"))
  expect_null(parse_dockerignore("  "))
  expect_null(parse_dockerignore(c("#foo", "  ", "# bar")))
})


test_that("parse_dockerignore: simple", {
  expect_equal(parse_dockerignore("hello"),
               list(patterns = "hello", is_exception = FALSE))
})


test_that("parse_dockerignore: exceptions", {
  expect_equal(parse_dockerignore(c("*.md", "!README.md")),
               list(patterns = c("*.md", "README.md"),
                    is_exception = c(FALSE, TRUE)))
})


test_that("parse_dockerignore: cleanup", {
  parse_dockerignore("//a///b\\c")$patterns

  expect_equal(parse_dockerignore("a/b/c")$patterns, "a/b/c")
  expect_equal(parse_dockerignore("a//b/c")$patterns, "a/b/c")
  expect_equal(parse_dockerignore("a//b\\c")$patterns, "a/b/c")
  expect_equal(parse_dockerignore("/a//b\\c")$patterns, "a/b/c")

  ## Lots of rubbish
  expect_equal(parse_dockerignore("////a/////b///c\\\\\\d")$patterns, "a/b/c/d")
})


test_that("simple cases", {
  paths <- c(paste0("dir1/", c("a.txt", "b.md", "c.c")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             paste0("dir3/", c("bar.md", "bar.c")),
             "README.md",
             "Dockerfile")
  root <- make_fake_files(paths)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dockerfile <- "Dockerfile"

  expect_equal(build_file_list(root, NULL), ".")

  dockerignore <- parse_dockerignore(c("*/*.md", "dir2"))
  expect_equal(
    build_file_list(root, dockerignore),
    sort(c("Dockerfile", "README.md", "dir1/a.txt", "dir1/c.c",
           "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("*/*.md", "dir2", "!dir2/foo.md"))
  expect_equal(
    build_file_list(root, dockerignore),
    sort(c("Dockerfile", "README.md", "dir1/a.txt", "dir1/c.c",
           "dir2/foo.md", "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("**/*.md", "dir2"))
  expect_equal(
    build_file_list(root, dockerignore),
    sort(c("Dockerfile", "dir1/a.txt", "dir1/c.c",
           "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("**/*.md", "dir2", "!dir2/foo.md"))
  expect_equal(
    build_file_list(root, dockerignore),
    sort(c("Dockerfile", "dir1/a.txt", "dir1/c.c",
           "dir2/foo.md", "dir3/bar.c")))
})


test_that("build file list with excluded dockerignore", {
  paths <- c(paste0("dir1/", c("a.txt", "Dockerfile")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             "README.md")
  root <- make_fake_files(paths)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dockerfile <- "dir1/Dockerfile"

  writeLines("dir1", file.path(root, ".dockerignore"))

  ignore1 <- parse_dockerignore("dir1")
  ignore2 <- include_dockerfile(ignore1, root, dockerfile)
  ignore3 <- read_dockerignore(root, dockerfile)
  expect_equal(ignore2, ignore3)

  expect_equal(build_file_list(root, ignore1),
               sort(c(".dockerignore", "dir2", "README.md")))
  expect_equal(build_file_list(root, ignore2),
               sort(c(".dockerignore", "dir1/Dockerfile", "dir2", "README.md")))
  expect_equal(build_file_list(root, ignore3),
               sort(c(".dockerignore", "dir1/Dockerfile", "dir2", "README.md")))
})


test_that("build_tar", {
  paths <- c(paste0("dir1/", c("a.txt", "Dockerfile")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             "README.md")
  root <- make_fake_files(paths)
  dockerfile <- "dir1/Dockerfile"

  x <- build_tar(root, dockerfile)
  expect_is(x, "raw")

  tmp <- untar_bin(x)

  expect_equal(dir(tmp, all.files = TRUE, recursive = TRUE),
               dir(root, all.files = TRUE, recursive = TRUE))
  unlink(tmp, recursive = TRUE)

  writeLines("dir2", file.path(root, ".dockerignore"))
  x <- build_tar(root, dockerfile)

  tmp2 <- untar_bin(x)

  f1 <- dir(tmp2, all.files = TRUE, recursive = TRUE)
  f2 <- dir(root, all.files = TRUE, recursive = TRUE)

  expect_equal(sort(f1), sort(grep("^dir2", f2, invert = TRUE, value = TRUE)))

  unlink(root, recursive = TRUE)
  unlink(tmp2, recursive = TRUE)
})


test_that("validate_tar_directory", {
  paths <- c(paste0("dir1/", c("a.txt", "Dockerfile")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             "README.md")
  root <- make_fake_files(paths)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dockerfile <- "dir1/Dockerfile"

  bin <- validate_tar_directory(root, dockerfile)
  expect_is(bin, "raw")

  expect_identical(validate_tar_directory(bin), bin)
})
