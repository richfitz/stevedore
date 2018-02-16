context("help generation")

test_that("all help generates", {
  topics <- list(NULL,
                 "containers",
                 "images",
                 "networks",
                 "volumes",
                 "docker_container",
                 "docker_image",
                 "docker_network",
                 "docker_volume",
                 "docker_exec")

  stevedore_reset()
  for (v in swagger_spec_versions()) {
    for (t in topics) {
      expect_silent(generate_help(t, v))
    }
  }
})

test_that("generate_help", {
  expect_equal(generate_help(NULL), generate_help_string(NULL))
  expect_error(generate_help_string("other"), "impossible!")
  expect_silent(generate_help("other"))
  expect_equal(generate_help("other"),
               "(automatic help generation has failed)", fixed = TRUE)
})


test_that("run help", {
  skip("might also work?")
  skip_on_cran()
  dest <- tempfile()
  cl <- test_docker_client()
  expect_error(cl$help("html"), NA)
})

test_that("generate help via utils::help", {
  ## I don't see why this fails with covr, but it does.
  skip("does not work in covr")
  skip_on_cran()
  dest <- tempfile()
  oo <- options(pager = fake_pager(dest))
  on.exit(options(oo))

  cl <- test_docker_client()
  cl$help()

  expect_true(file.exists(dest))
  txt <- readLines(dest)
  expect_match(txt, "Below is reference documentation for all methods",
               fixed = TRUE, all = FALSE)
})
