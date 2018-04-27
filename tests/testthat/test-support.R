context("support")

test_that("as_docker_filter", {
  fil <- c(1:10)
  expect_error(as_docker_filter(fil), "'fil' must be named")
  fil <- c(a = 1, b = 2)
  expect_error(as_docker_filter(fil), "'fil' must be a character")

  expect_null(as_docker_filter(NULL))

  expect_equal(as.character(as_docker_filter(c(a = "foo"))),
               '{"a":["foo"]}')
  expect_equal(as.character(as_docker_filter(list(a = "foo"))),
               '{"a":["foo"]}')
  expect_equal(as.character(as_docker_filter(list(a = c("foo", "bar")))),
               '{"a":["foo","bar"]}')

  ## Pass json through untouched
  tmp <- jsonlite::toJSON(mtcars)
  expect_identical(as_docker_filter(tmp), tmp)
})


test_that("as_body_array_string", {
  expect_identical(as_body_array_string("a"), "a")
  expect_identical(as_body_array_string(character()), character())
  expect_identical(as_body_array_string(letters), letters)
  expect_error(as_body_array_string(1), "'1' must be a character", fixed = TRUE)
})


test_that("as_string_map", {
  expect_null(as_string_map(NULL))
  expect_equal(as_string_map(c(a = "A")), list(a = jsonlite::unbox("A")))
  expect_equal(as_string_map(c(a = "A", b = "B")),
               list(a = jsonlite::unbox("A"), b = jsonlite::unbox("B")))

  expect_error(as_string_map(c(a = "A", a = "B")), "unique names")
  expect_error(as_string_map(c("A", a = "B")), "named")
  expect_error(as_string_map(c(a = 1, b = 2)), "character")
})


test_that("docker_client_method_unsupported", {
  endpoint <- list(name = "foo",
                   method = "post",
                   path = "/path",
                   version_required = "1.2.3",
                   version_used = "0.0.1")
  ## TODO: the name here must match an name in inst/spec/help.yaml:unsupported
  m <- docker_client_method_unsupported(endpoint, "image_build_clean")
  expect_is(m, "docker_client_method")
  expect_error(
    m(),
    "'foo' (post /path) requires docker API version at least 1.2.3 (version 0.0.1 used)",
    fixed = TRUE)

  s1 <- paste(crayon::strip_style(capture.output(print(m))), collapse = "\n")
  s2 <- paste(crayon::strip_style(format(m)), collapse = "\n")

  expect_equal(s1, s2)
})


test_that("docker_api_client_auth", {
  auth <- docker_api_client_auth()
  str <- "{a:1}"
  expect_null(auth$get("foo"))
  auth$set("foo", str)
  expect_equal(auth$get("foo"), base64encode(str))
})


test_that("client request", {
  r <- function(verb, path, query, body, headers, stream) {
    list(verb = verb, path = path, query = query, body = body,
         headers = headers, stream = stream)
  }
  client <- list(.api_client = list(http_client = list(request = r)))
  request <- make_docker_client_request(client)

  expect_equal(request("GET", "/path"),
               list(verb = "GET", path = "/path", query = NULL, body = NULL,
                    headers = NULL, stream = NULL))
  expect_equal(request("get", "/path"),
               list(verb = "GET", path = "/path", query = NULL, body = NULL,
                    headers = NULL, stream = NULL))

  expect_equal(request("GET", "/path", list(a = 1)),
               list(verb = "GET", path = "/path", query = list(a = 1),
                    body = NULL, headers = NULL, stream = NULL))
  expect_error(request("GET", "/path", 1),
               "'query' must be a list")

  expect_equal(request("GET", "/path", body = raw(10)),
               list(verb = "GET", path = "/path", query = NULL,
                    body = raw(10), headers = NULL, stream = NULL))

  expect_equal(request("GET", "/path", stream = identity),
               list(verb = "GET", path = "/path", query = NULL,
                    body = NULL, headers = NULL, stream = identity))
  expect_error(request("GET", "/path", stream = TRUE),
               "'stream' must be a function")
})
