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

  for (v in swagger_spec_versions()) {
    for (t in topics) {
      expect_silent(generate_help_string(t, v))
    }
  }
})
