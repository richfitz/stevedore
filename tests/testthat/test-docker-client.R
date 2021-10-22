context("docker client")


test_that("unknown args prevented", {
  expect_error(docker_client(foo = "bar"),
               "Unknown argument passed to 'docker_client': foo")
})


## The most simple nontrivial thing with the docker client
test_that("ping", {
  d <- test_docker_client()
  expect_is(d, "stevedore_object")
  expect_is(d, "docker_client")
  ok <- d$ping()
  expect_equivalent(ok, "OK")
  expect_equal(names(attributes(ok)),
               c("api_version", "docker_experimental"))
})


test_that("connection_info", {
  cl <- null_docker_client()
  res <- cl$connection_info()
  expect_is(res, "list")
  expect_equal(names(res), c("api_version", "protocol", "host",
                             "http_client_type", "use_tls",
                             "tls_verify", "tls_certificates"))
  expect_equal(res$http_client_type, "null")
})


test_that("info", {
  d <- test_docker_client()
  info <- d$info()
  expect_is(info, "list")
})


test_that("version", {
  d <- test_docker_client()
  v <- d$version()
  expect_is(v, "list")
  expect_is(v$api_version, "character")
})


test_that("df", {
  skip("df containers output looks broken to me")
  d <- test_docker_client()
  df <- d$df()
})


test_that("events", {
  skip("events this needs some streaming support")
  d <- test_docker_client()
})


test_that("children", {
  d <- null_docker_client()
  expect_is(d$container, "docker_container_collection")
  expect_is(d$container, "stevedore_object")

  expect_is(d$image, "docker_image_collection")
  expect_is(d$image, "stevedore_object")

  expect_is(d$network, "docker_network_collection")
  expect_is(d$network, "stevedore_object")

  expect_is(d$volume, "docker_volume_collection")
  expect_is(d$volume, "stevedore_object")

  expect_is(d$swarm, "docker_swarm_collection")
  expect_is(d$swarm, "stevedore_object")

  expect_is(d$node, "docker_node_collection")
  expect_is(d$node, "stevedore_object")

  expect_is(d$service, "docker_service_collection")
  expect_is(d$service, "stevedore_object")

  expect_is(d$task, "docker_task_collection")
  expect_is(d$task, "stevedore_object")

  expect_is(d$secret, "docker_secret_collection")
  expect_is(d$secret, "stevedore_object")

  expect_is(d$config, "docker_config_collection")
  expect_is(d$config, "stevedore_object")

  expect_is(d$plugin, "docker_plugin_collection")
  expect_is(d$plugin, "stevedore_object")
})


test_that("Prevent invalid access", {
  d <- null_docker_client()
  expect_error(d$foo, "No element 'foo' within 'docker_client' object")
  expect_error(d[["foo"]], "No element 'foo' within 'docker_client' object")
  expect_error(d[[1]], "'i' must be a scalar character (non-NA)", fixed = TRUE)

  expect_error(d$container$foo,
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d[["container"]][["foo"]],
               "No element 'foo' within 'docker_container_collection' object")
  expect_error(d$container[[1]], "'i' must be a scalar character (non-NA)",
               fixed = TRUE)
})


test_that("print", {
  d <- null_docker_client()
  out <- capture.output(x <- print(d))
  expect_identical(x, d)
  expect_true(any(out == "<docker_client>"))
  expect_true(any(out == "  ping()"))

  out <- capture.output(x <- print(d$container))
  expect_identical(x, d$container)
  expect_true(any(out == "<docker_container_collection>"))
  expect_true(any(out == "  get(id)"))
})


test_that("api_version", {
  d <- null_docker_client()
  expect_equal(d$api_version(), DOCKER_API_VERSION_DEFAULT)
})


test_that("print endpoints", {
  d <- null_docker_client()
  expect_is(d$ping, "docker_client_method")
  txt <- capture.output(print(d$ping))
  expect <- c(
    "function()",
    "----------",
    "Ping. This is a dummy endpoint you can use to test if the server is",
    "  accessible.")
  expect_equal(txt[1:4], expect) # sometimes there is a blank line here

  txt <- capture_output_no_crayon(print(d$login))
  expect_match(txt, "Check auth configuration",
               all = FALSE, fixed = TRUE)
  expect_match(txt, "  email: For authentication to check",
               all = FALSE, fixed = TRUE)

  txt <- capture_output_no_crayon(print(d$events))
  expect_match(
    txt,
    "        - `type=<string>` object to filter by, one of `container`,",
    all = FALSE, fixed = TRUE)

  txt <- capture_output_no_crayon(print(d$login))
  expect_match(
    txt,
    "Check auth configuration. Validate credentials for a registry and, if",
    all = FALSE, fixed = TRUE)

  txt <- capture_output_no_crayon(print(d$container$create))
  expect_match(
    txt,
    "  shell: Shell for when `RUN`, `CMD`, and `ENTRYPOINT` uses a shell",
    all = FALSE, fixed = TRUE)
})


test_that("login", {
  skip("not yet tested")
})


test_that("events", {
  skip("not yet tested")
})


test_that("direct access", {
  cl <- test_docker_client()

  res <- cl$request("GET", "/_ping")
  expect_equal(res$content, charToRaw("OK"))
})


test_that("custom data.frame handler is enabled", {
  df <- function(x) {
    class(x) <- c("foo", class(x))
    x
  }
  cl <- test_docker_client(data_frame = df)

  res <- cl$container$list()
  expect_is(res, "foo")
  expect_is(res, "data.frame")
})


test_that("cp", {
  nm <- rand_str(10, "stevedore_")
  cl <- test_docker_client()
  x <- cl$container$run("richfitz/iterate", cmd = c("100", "100"),
                        name = nm, rm = TRUE, detach = TRUE)
  on.exit(x$kill())

  tmp <- tempfile()
  cl$cp(sprintf("%s:/usr/local/bin/iterate", nm), tmp)
  expect_equal(readLines(tmp), readLines("images/iterate/iterate"))

  cl$cp(tmp, sprintf("%s:/copy", nm))

  res <- as.character(x$exec(c("cat", "/copy"), stdout = TRUE, stderr = FALSE,
                             stream = FALSE)$output)
  expect_equal(strsplit(res, "\n", fixed = TRUE)[[1]],
               readLines(tmp))

  expect_error(cl$cp("foo", "bar"),
               "must specify at least one container source",
               fixed = TRUE)
  expect_error(cl$cp(sprintf("%s:foo", nm), sprintf("%s:bar", nm)),
               "copying between containers is not supported",
               fixed = TRUE)
})


test_that("debug http", {
  cl <- test_docker_client(debug = TRUE)
  txt <- capture.output(res <- cl$ping())
  expect_match(tolower(txt), "user-agent", fixed = TRUE, all = FALSE)
})
