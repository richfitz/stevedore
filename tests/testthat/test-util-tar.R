context("util (tar)")

test_that("tar_directory", {
  p <- tempfile()
  dir.create(p, TRUE, FALSE)
  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_directory(p)
  expect_is(bin, "raw")
  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters))
})

test_that("selective tar", {
  p <- tempfile()
  dir.create(p, TRUE, FALSE)

  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_files(letters[1:6], p)

  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters[1:6]))
})

test_that("tar_file", {
  bin <- tar_file("sample_responses/README.md")
  tmp <- untar_bin(bin)
  expect_equal(dir(tmp), "README.md")
  expect_equal(unname(tools::md5sum(file.path(tmp, "README.md"))),
               unname(tools::md5sum("sample_responses/README.md")))
})


test_that("tar failure", {
  expect_error(tar_system(tempfile(), tempfile()),
               "tar failed with status")
})


test_that("external list", {
  p <- tempfile()
  dir.create(p, TRUE, FALSE)
  for (i in letters) {
    writeLines(i, file.path(p, i))
  }

  bin <- tar_files(letters[1:6], p, external_list = TRUE)
  tmp <- untar_bin(bin)
  expect_true(setequal(dir(tmp), letters[1:6]))
})
