context("util (tar)")


test_that("tar_directory", {
  p <- tempfile_test()
  dir.create(p, TRUE, FALSE)
  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_directory(p)
  expect_is(bin, "raw")
  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters))

  unlink(tmp, recursive = TRUE)
  unlink(p, recursive = TRUE)
})


test_that("selective tar", {
  p <- tempfile_test()
  dir.create(p, TRUE, FALSE)

  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_files(letters[1:6], p)

  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters[1:6]))

  unlink(tmp, recursive = TRUE)
  unlink(p, recursive = TRUE)
})


test_that("tar_file", {
  bin <- tar_file("sample_responses/README.md")
  tmp <- untar_bin(bin)
  expect_equal(dir(tmp), "README.md")
  expect_equal(unname(tools::md5sum(file.path(tmp, "README.md"))),
               unname(tools::md5sum("sample_responses/README.md")))
  unlink(tmp, recursive = TRUE)
})


test_that("tar failure", {
  p1 <- tempfile_test()
  p2 <- tempfile_test()
  expect_error(tar_system(p1, p2),
               "tar failed with status")
  unlink(p1)
})


test_that("external list", {
  p <- tempfile_test()
  dir.create(p, TRUE, FALSE)
  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_files(letters[1:6], p, external_list = TRUE)
  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters[1:6]))

  unlink(tmp, recursive = TRUE)
  unlink(p, recursive = TRUE)
})
