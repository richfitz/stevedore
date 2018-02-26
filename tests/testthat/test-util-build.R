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
  dockerfile <- "Dockerfile"

  expect_equal(build_file_list(root, NULL, dockerfile), ".")

  dockerignore <- parse_dockerignore(c("*/*.md", "dir2"))
  expect_equal(
    build_file_list(root, dockerignore, dockerfile),
    sort(c("Dockerfile", "README.md", "dir1/a.txt", "dir1/c.c",
           "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("*/*.md", "dir2", "!dir2/foo.md"))
  expect_equal(
    build_file_list(root, dockerignore, dockerfile),
    sort(c("Dockerfile", "README.md", "dir1/a.txt", "dir1/c.c",
           "dir2/foo.md", "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("**/*.md", "dir2"))
  expect_equal(
    build_file_list(root, dockerignore, dockerfile),
    sort(c("Dockerfile", "dir1/a.txt", "dir1/c.c",
           "dir3/bar.c")))

  dockerignore <- parse_dockerignore(c("**/*.md", "dir2", "!dir2/foo.md"))
  expect_equal(
    build_file_list(root, dockerignore, dockerfile),
    sort(c("Dockerfile", "dir1/a.txt", "dir1/c.c",
           "dir2/foo.md", "dir3/bar.c")))
})


test_that("build file list with excluded dockerignore", {
  paths <- c(paste0("dir1/", c("a.txt", "Dockerfile")),
             paste0("dir2/", c("file.txt", "foo.md", "secret.json")),
             "README.md")
  root <- make_fake_files(paths)
  dockerfile <- "dir1/Dockerfile"

  dockerignore <- parse_dockerignore("dir1")
  expect_equal(build_file_list(root, dockerignore, dockerfile),
               sort(c("dir1/Dockerfile", "dir2", "README.md")))
})
