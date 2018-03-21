context("secrets")

test_that("create and delete", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  ## This set of tests is highly version specific with at three
  ## patches so I am going to test it over all supported versions:
  for (v in swagger_spec_versions()) {
    cl_v <- test_docker_client(api_version = v)

    data <- base64encode("secret!", TRUE)
    key <- rand_str()
    id <- cl$secrets$create(key, data)
    expect_is(id, "character")
    expect_true(id %in% cl$secrets$list()$id)
    expect_true(key %in% cl$secrets$list()$name)

    dat <- cl$secrets$inspect(id)
    expect_identical(cl$secrets$inspect(key), dat)

    expect_null(cl$secrets$remove(key))
    expect_false(id %in% cl$secrets$list()$id)
    expect_false(key %in% cl$secrets$list()$name)
  }
})
