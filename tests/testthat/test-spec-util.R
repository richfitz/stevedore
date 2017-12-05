context("spec utilites")

test_that("startup", {
  stevedore:::.onLoad()
  expect_equal(.stevedore$client_data, list())
})

test_that("read missing spec is an error", {
  expect_error(read_spec("0.0.1"),
               "Invalid version 0.0.1; try one of", fixed = TRUE)
})

test_that("validate", {
  skip_if_not_installed("withr")
  tmp <- tempfile()
  dir.create(tmp)
  file.copy(dir(spec_path(), full.names = TRUE), tmp)

  version <- DEFAULT_DOCKER_API_VERSION

  cmp <- read_spec(version)

  withr::with_options(list(stevedore.spec.path = tmp),
                      expect_equal(read_spec(version), cmp))

  filename <- file.path(tmp, sprintf("v%s.yaml", version))
  txt <- readLines(filename)
  writeLines(c("#", txt), filename)

  withr::with_options(
    list(stevedore.spec.path = tmp),
    expect_error(read_spec(version, TRUE),
                 "Spec for 1.29 had different md5 than expected"))
})

test_that("write_spec_index", {
  skip_if_not_installed("withr")
  tmp <- tempfile()
  dir.create(tmp)
  file.copy(dir(spec_path(), full.names = TRUE), tmp)
  cmp <- yaml::yaml.load_file(
    system.file("spec/index.yaml", package = "stevedore", mustWork = TRUE))
  write_spec_index(tmp)
  filename <- file.path(tmp, "index.yaml")
  expect_true(file.exists(filename))
  expect_equal(yaml::yaml.load_file(filename), cmp)
})
