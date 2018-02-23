context("dockerignore")

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
