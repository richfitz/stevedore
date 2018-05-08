context("docker client: nodes")


test_that("create (offline)", {
  d <- null_docker_client()
  n <- d$node$get(dummy_id())
  expect_is(n, "docker_node")
  expect_equal(n$id(), dummy_id())
})


test_that("basic node", {
  cl <- test_docker_client()

  id <- cl$swarm$init()
  on.exit(cl$swarm$leave(TRUE))

  nodes <- cl$node$list()
  expect_equal(nrow(nodes), 1L)

  node <- cl$node$get(nodes$id)

  expect_is(node, "docker_node")
  expect_equal(node$id(), nodes$id)
  expect_is(node$hostname(), "character")
  expect_is(node$version(), "integer")
  expect_equal(node$status(), "ready")
  expect_equal(node$role(), "manager")
  expect_equal(node$availability(), "active")
})
