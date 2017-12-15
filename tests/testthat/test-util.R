context("util")

test_that("camel <-> snake", {
  expect_equal(camel_to_snake("fooBar"), "foo_bar")
  expect_equal(camel_to_snake("foo_bar"), "foo_bar")
  expect_equal(snake_to_camel("fooBar"), "fooBar")
  expect_equal(snake_to_camel("foo_bar"), "fooBar")

  expect_equal(camel_to_snake(c("fooBar", "fizzBuzz")),
               c("foo_bar", "fizz_buzz"))
  expect_equal(snake_to_camel(c("foo_bar", "fizz_buzz")),
               c("fooBar", "fizzBuzz"))
})

test_that("case convert with consecutive capitals", {
  expect_equal(pascal_to_snake("NanoCPUs"), "nano_cpus")
  ## The conversion is lossy though:
  expect_equal(snake_to_pascal("nano_cpus"), "NanoCpus")
})

test_that("case convert: reference check", {
  nms <- read.csv("names.csv", stringsAsFactors = FALSE)
  expect_equal(nms$to, pascal_to_snake(nms$from))
})

test_that("case convert: special cases", {
  ## These were derived by doing:
  ##
  ##   nms <- read.csv("names.csv", stringsAsFactors = FALSE)
  ##   tmp <- nms[grepl("^[A-Z]{2}", nms$from), ]
  ##   writeLines(sprintf('expect_equal(pascal_to_snake("%s"), "%s")',
  ##                      tmp$from, tmp$to))
  ##
  ## Other tricky cases can be added directly.
  expect_equal(pascal_to_snake("CACert"), "ca_cert")
  expect_equal(pascal_to_snake("CAConfig"), "ca_config")
  expect_equal(pascal_to_snake("CPUSet"), "cpu_set")
  expect_equal(pascal_to_snake("CPUShares"), "cpu_shares")
  expect_equal(pascal_to_snake("ID"), "id")
  expect_equal(pascal_to_snake("IOMaximumBandwidth"), "io_maximum_bandwidth")
  expect_equal(pascal_to_snake("IOMaximumIOps"), "io_maximum_iops")
  expect_equal(pascal_to_snake("IP"), "ip")
  expect_equal(pascal_to_snake("IPAddress"), "ip_address")
  expect_equal(pascal_to_snake("IPAM"), "ipam")
  expect_equal(pascal_to_snake("IPAMConfig"), "ipam_config")
  expect_equal(pascal_to_snake("IPPrefixLen"), "ip_prefix_len")
  expect_equal(pascal_to_snake("IPv4Address"), "ipv4_address")
  expect_equal(pascal_to_snake("IPv4Forwarding"), "ipv4_forwarding")
  expect_equal(pascal_to_snake("IPv6Address"), "ipv6_address")
  expect_equal(pascal_to_snake("IPv6Gateway"), "ipv6_gateway")
  expect_equal(pascal_to_snake("NCPU"), "n_cpu")
  expect_equal(pascal_to_snake("NEventsListener"), "n_events_listener")
  expect_equal(pascal_to_snake("NFd"), "n_fd")
  expect_equal(pascal_to_snake("NGoroutines"), "n_goroutines")
  expect_equal(pascal_to_snake("OOMKilled"), "oom_killed")
  expect_equal(pascal_to_snake("OSType"), "os_type")
  expect_equal(pascal_to_snake("RW"), "rw")
  expect_equal(pascal_to_snake("TLSInfo"), "tls_info")
  expect_equal(pascal_to_snake("URL"), "url")
  expect_equal(pascal_to_snake("UTSMode"), "uts_mode")
})

test_that("is_error", {
  expect_false(is_error(NULL))

  cond <- list(message = "foo", code = 404L, endpoint = "pull")
  class(cond) <- c("docker_error", "error", "condition")
  expect_true(is_error(cond))
})

test_that("split command", {
  expect_identical(split_command("hello"), "hello")
  expect_identical(split_command("hello world"), c("hello", "world"))

  ## These we don't handle yet:
  expect_error(split_command("'foo bar'"), "A proper command splitter")
  expect_error(split_command("foo 'bar'"), "A proper command splitter")
  expect_error(split_command('foo "bar"'), "A proper command splitter")
  expect_error(split_command('"foo bar"'), "A proper command splitter")
})

test_that("check command", {
  expect_identical(check_command("hello world"), "hello world")
  expect_identical(check_command(I("hello world")), c("hello", "world"))
  expect_null(check_command(NULL))
  expect_identical(check_command(letters), letters)
  expect_identical(check_command(I(letters)), I(letters))
})

## The new yaml package introduces integer overflow with warnings.
test_that("yaml overflow", {
  str <- "Resources:\n  NanoCPUs: 4000000000"
  expect_identical(yaml_load(str),
                   list(Resources = list(NanoCPUs = 4e9)))
})

test_that("stream filtering", {
  s <- rep(1:2, length.out = 10)
  x <- paste0(letters[1:10], "\n")
  obj <- docker_stream(x, s)

  expect_equal(format(obj, style = "plain", filter = "stdout"), x[s == 1])
  expect_equal(format(obj, style = "plain", filter = "stderr"), x[s == 2])
  expect_equal(format(obj, style = "plain", filter = c("stdout", "stderr")), x)
  expect_equal(format(obj, style = "plain", filter = c("stdin")), character(0))
  expect_equal(format(obj, style = "plain", filter = NULL), x)
})
