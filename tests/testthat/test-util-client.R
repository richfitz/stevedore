context("util (client)")

test_that("report warnings", {
  expect_silent(report_warnings(character(0), "doing a thing"))
  expect_warning(report_warnings("my warning", "doing a thing"),
                 "1 warning produced while doing a thing.+- my warning")
  expect_warning(report_warnings(c("my warning", "another warning"),
                                 "doing a thing"),
                 paste0("2 warnings produced while doing a thing.+",
                        "- my warning.+",
                        "- another warning"))
})

test_that("pull status", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- pull_status_printer(con)

  txt <- readLines("sample_responses/pull/ubuntu")
  cmp <- readLines("sample_responses/pull/ubuntu.out")
  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  expect_equal(res, cmp)
})

test_that("pull status: silent error", {
  path <- tempfile()
  con <- file(path, "w+")
  on.exit(close(con))
  p <- pull_status_printer(con)

  txt <- readLines("sample_responses/pull/hello-world-macos")
  cmp <- readLines("sample_responses/pull/hello-world-macos.out")
  for (i in txt) {
    p(from_json(i))
  }
  close(con)
  on.exit()

  res <- readLines(path)
  expect_equal(res, cmp)
})
