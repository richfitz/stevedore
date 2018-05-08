context("help generation")


test_that("all help generates", {
  topics <- list(NULL,
                 "container",
                 "image",
                 "network",
                 "volume",
                 "swarm",
                 "node",
                 "service",
                 "task",
                 "secret",
                 "config",
                 "plugin",
                 "docker_container",
                 "docker_image",
                 "docker_network",
                 "docker_volume",
                 "docker_node",
                 "docker_service",
                 "docker_task",
                 "docker_plugin",
                 "docker_exec")

  stevedore_reset()
  for (v in swagger_spec_versions()) {
    for (t in topics) {
      expect_silent(generate_help(t, v))
      expect_silent(generate_help_string(t, v))
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
  dest <- tempfile_test()
  cl <- test_docker_client()
  expect_error(cl$help("html"), NA)
})


test_that("generate help via utils::help", {
  ## I don't see why this fails with covr, but it does.
  skip("does not work in covr")
  skip_on_cran()
  dest <- tempfile_test()
  oo <- options(pager = fake_pager(dest))
  on.exit(options(oo))

  cl <- test_docker_client()
  cl$help()

  expect_true(file.exists(dest))
  txt <- readLines(dest)
  expect_match(txt, "Below is reference documentation for all methods",
               fixed = TRUE, all = FALSE)
})


## Help does not break object load
test_that("construct all api versions", {
  for (v in swagger_spec_versions()) {
    ## This does the core object and collections:
    d <- null_docker_client(api_version = v)
    expect_equal(d$api_version(), v)

    ## then the special objects:
    expect_silent(docker_container(dummy_id(), d))
    expect_silent(docker_image(dummy_id(), d))
    expect_silent(docker_network(dummy_id(), d))
    expect_silent(docker_volume(dummy_id(), d))
    expect_silent(docker_exec(dummy_id(), d))

    expect_silent(docker_node(dummy_id(), d))
    expect_silent(docker_service(dummy_id(), d))
    expect_silent(docker_task(dummy_id(), d))
    expect_silent(docker_plugin(dummy_id(), d))
  }
})


test_that("format one method", {
  d <- null_docker_client()
  s1 <- format(d$ping)
  s2 <- format(d$ping, type = "text")
  s3 <- format(d$ping, type = "rd")
  expect_identical(s1, s2)
  expect_false(identical(s3, s1))
})


test_that("markdown_to_text", {
  expect_equal(markdown_to_text("hello `code` world", FALSE),
               "hello `code` world")
  expect_equal(markdown_to_text("hello ```code``` world", FALSE),
               "hello ```code``` world")
  expect_equal(markdown_to_text("hello `code` world", TRUE),
               "hello \033[1mcode\033[22m world")
  expect_equal(markdown_to_text("hello ```code``` world", TRUE),
               "hello \033[1mcode\033[22m world")
})


test_that("version remembering", {
  set_help_api_last_version("1.32")
  expect_equal(get_help_api_last_version(), "1.32")
  set_help_api_last_version("1.33")
  expect_equal(get_help_api_last_version(), "1.33")
  set_help_api_last_version(NULL)
})
