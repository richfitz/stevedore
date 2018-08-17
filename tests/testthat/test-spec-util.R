context("spec utilites")


test_that("startup", {
  stevedore:::.onLoad()
  expect_equal(.stevedore$client_data, list())
})


test_that("read missing spec is an error", {
  expect_error(swagger_spec_read("0.0.1"),
               "Invalid version 0.0.1; try one of", fixed = TRUE)
})


test_that("validate", {
  skip_if_not_installed("withr")
  tmp <- tempfile_test()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(dir(swagger_spec_path(), full.names = TRUE), tmp)

  version <- DOCKER_API_VERSION_DEFAULT

  cmp <- swagger_spec_read(version)

  withr::with_options(list(stevedore.spec.path = tmp),
                      expect_equal(swagger_spec_read(version), cmp))

  filename <- file.path(tmp, sprintf("v%s.yaml", version))
  txt <- readLines(filename)
  writeLines(c("#", txt), filename)

  withr::with_options(
    list(stevedore.spec.path = tmp),
    expect_error(swagger_spec_read(version, refresh = TRUE),
                 "Spec for 1.29 had different md5 than expected"))
})
