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
  nms <- read.csv(stevedore_file("spec/names.csv"), stringsAsFactors = FALSE)
  ex <- read.csv(stevedore_file("spec/names_override.csv"),
                 stringsAsFactors = FALSE)
  i <- !(nms$from %in% ex$from)
  expect_equal(nms$to[i], pascal_to_snake(nms$from[i]))
})


test_that("case convert: caching", {
  pascal_to_snake_cache_reset()
  expect_equal(pascal_to_snake("CACert"),
               pascal_to_snake_cached("CACert"))

  from <- "AbraCadabra"
  to <- pascal_to_snake(from)
  expect_false(from %in% .stevedore$names[, "from"])
  expect_false(to %in% .stevedore$names[, "to"])
  expect_equal(pascal_to_snake_cached(from), to)
  expect_true(from %in% .stevedore$names[, "from"])
  expect_true(to %in% .stevedore$names[, "to"])
})


test_that("case convert: special cases", {
  ## See notes in design.md
  expect_equal(pascal_to_snake("CACert"), "ca_cert")
  expect_equal(pascal_to_snake("CAConfig"), "ca_config")
  expect_equal(pascal_to_snake("CPUSet"), "cpu_set")
  expect_equal(pascal_to_snake("CPUShares"), "cpu_shares")
  expect_equal(pascal_to_snake("DNSConfig"), "dns_config")
  expect_equal(pascal_to_snake("GID"), "gid")
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
  expect_equal(pascal_to_snake("OS"), "os")
  expect_equal(pascal_to_snake("OSFeatures"), "os_features")
  expect_equal(pascal_to_snake("OSType"), "os_type")
  expect_equal(pascal_to_snake("OSVersion"), "os_version")
  expect_equal(pascal_to_snake("PID"), "pid")
  expect_equal(pascal_to_snake("RW"), "rw")
  expect_equal(pascal_to_snake("SELinuxContext"), "selinux_context")
  expect_equal(pascal_to_snake("TLSInfo"), "tls_info")
  expect_equal(pascal_to_snake("TTY"), "tty")
  expect_equal(pascal_to_snake("UID"), "uid")
  expect_equal(pascal_to_snake("URL"), "url")
  expect_equal(pascal_to_snake("URLs"), "urls")
  expect_equal(pascal_to_snake("UTSMode"), "uts_mode")
})


test_that("case convert: exceptions", {
  expect_equal(pascal_to_snake("MinAPIVersion"), "min_apiversion")
  expect_equal(pascal_to_snake_cached("MinAPIVersion"), "min_api_version")
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
  expect_identical(validate_command("hello world"), "hello world")
  expect_identical(validate_command(I("hello world")), c("hello", "world"))
  expect_null(validate_command(NULL))
  expect_identical(validate_command(letters), letters)
  expect_identical(validate_command(I(letters)), I(letters))
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


test_that("stream truncating", {
  s <- rep(1:2, length.out = 10)
  x <- paste0(letters[1:10], "\n")
  obj <- docker_stream(x, s)

  expect_equal(
    format(obj, style = "prefix", strip_newline = TRUE, max_lines = 3),
    c("O> a", "-- [...truncated 8 lines...]", "E> j"))
  expect_equal(
    format(obj, style = "prefix", strip_newline = TRUE, max_lines = 4),
    c("O> a", "-- [...truncated 7 lines...]", "O> i", "E> j"))
  expect_equal(
    format(obj, style = "prefix", strip_newline = TRUE, max_lines = 10),
    format(obj, style = "prefix", strip_newline = TRUE))
})


test_that("stream printing", {
  x <- sprintf("Reticulating spline %d...\n", 1:10)
  s <- rep(1, length.out = length(x))
  logs <- docker_stream(x, s)

  expect_match(format(logs, style = "plain"),
               "^Reticulating spline \\d+...\n$")
  expect_match(format(logs, style = "prefix"),
               "^O> Reticulating spline \\d+...\n")
  expect_equal(all(crayon::has_style(format(logs, style = "colour"))),
               crayon::has_color())
  expect_match(capture.output(print(logs, style = "plain")),
               "^Reticulating spline \\d+...$")
  expect_match(capture.output(print(logs, style = "prefix")),
               "^O> Reticulating spline \\d+...$")
  expect_match(capture.output(print(logs)), "Reticulating spline \\d+...")
})


test_that("container output", {
  x <- list(id = function() "aaa", name = function() "bbb")
  class(x) <- "docker_container"
  expect_equal(format(x),
               c("<docker_container>",
                 "  id: aaa",
                 "  name: bbb"))
})


test_that("container output print", {
  x <- list(container = "a", logs = "b")
  class(x) <- "docker_run_output"
  str <- c("<docker_run_output>",
           "  $container:",
           "    a",
           "",
           "  $logs:",
           "    b")
  expect_equal(format(x), str)
  expect_equal(capture.output(res <- withVisible(print(x))), str)
  expect_identical(res, list(value = x, visible = FALSE))
})


test_that("integer apply", {
  int <- integer(1)
  twice <- function(x) {
    x * (if (is.integer(x)) 2L else 2.0)
  }
  ## Basic mode: this is all ok
  x <- list(a = 100L, b = 200L)
  expect_identical(viapply(x, twice), c(a = 200L, b = 400L))
  expect_identical(viapply(x, twice, USE.NAMES = FALSE), c(200L, 400L))

  expect_identical(vapply2(x, twice, int), c(a = 200L, b = 400L))
  expect_identical(vapply2(x, twice, int, USE.NAMES = FALSE), c(200L, 400L))


  ## How about integer-as-numeric?
  x <- lapply(x, as.numeric)
  expect_identical(viapply(x, twice), c(a = 200L, b = 400L))
  expect_identical(viapply(x, twice, USE.NAMES = FALSE), c(200L, 400L))
  expect_identical(vapply2(x, twice, int), c(a = 200L, b = 400L))
  expect_identical(vapply2(x, twice, int, USE.NAMES = FALSE), c(200L, 400L))

  ## Very large numbers:
  large <- .Machine$integer.max + 1.0
  x <- list(a = 100L, b = large)
  expect_identical(viapply(x, twice), c(a = 200.0, b = large * 2))
  expect_identical(viapply(x, twice, USE.NAMES = FALSE),
                   c(200.0, large * 2))
  expect_identical(vapply2(x, twice, int), c(a = 200.0, b = large * 2))
  expect_identical(vapply2(x, twice, int, USE.NAMES = FALSE),
                   c(200.0, large * 2))

  ## Error case:
  expect_error(viapply(x, function(x) x + 0.1),
               "Result not integer-like")

  ## non-integer
  expect_identical(
    vapply2(letters, strrep, character(1), 2, USE.NAMES = FALSE),
    strrep(letters, 2))
})


test_that("integer apply/json serialisation", {
  fmt <- '[{"a": %s}, {"a": %s}]'
  s1 <- sprintf(fmt, "100", "200")
  s2 <- sprintf(fmt, "10000000000", "20000000000")
  expect_identical(viapply(from_json(s1), "[[", "a"), c(100L, 200L))
  expect_identical(viapply(from_json(s2), "[[", "a"), (1:2) * 10^10)
})


test_that("sys_which", {
  expect_silent(p <- sys_which("ping"))
  expect_is(p, "character")
  expect_identical(p, unname(Sys.which("ping")))

  expect_error(sys_which("unknown-program-never-exists"),
               "Did not find program 'unknown-program-never-exists'")
})


test_that("reset_line", {
  tmp <- tempfile_test()
  con <- file(tmp, "w+")
  on.exit(close(con))

  cat("hello", file = con)
  reset_line(con, 10, is_tty = TRUE)
  cat("goodbye", file = con)
  close(con)
  on.exit()

  bytes <- readBin(tmp, raw(), file.size(tmp))
  expect_equal(rawToChar(bytes),
               sprintf("hello\r%s\rgoodbye", strrep(" ", 10)))
  unlink(tmp)

  tmp <- tempfile_test()
  con <- file(tmp, "w+")
  on.exit(close(con))
  cat("hello", file = con)
  reset_line(con, 10, is_tty = FALSE, newline_if_not_tty = TRUE)
  cat("good", file = con)
  reset_line(con, 10, is_tty = FALSE, newline_if_not_tty = FALSE)
  cat("bye", file = con)
  close(con)
  on.exit()

  bytes <- readBin(tmp, raw(), file.size(tmp))
  expect_equal(rawToChar(bytes), "hello\ngoodbye")
  unlink(tmp)
})


test_that("download_file", {
  d <- test_docker_client()
  x <- d$container$run("nginx", detach = TRUE, ports = TRUE, rm = TRUE)
  on.exit(x$stop(0))

  url <- sprintf("http://localhost:%s/index.html", x$ports()$host_port)
  p <- tempfile_test()

  expect_silent(cmp <- download_file(url, p, quiet = TRUE))
  expect_identical(cmp, p)

  txt <- readLines(p)
  expect_match(txt, "Welcome to nginx", fixed = TRUE, all = FALSE)
  writeLines("", p)

  expect_silent(cmp <- download_file(url, p, quiet = TRUE))
  expect_equal(readLines(p), "")

  unlink(p)
})


test_that("pretty_bytes", {
  expect_equal(pretty_bytes(1), "1 B")
  expect_equal(pretty_bytes(12), "12 B")
  expect_equal(pretty_bytes(123), "123 B")
  expect_equal(pretty_bytes(1234), "1.23 kB")
  expect_equal(pretty_bytes(12345), "12.35 kB")
  expect_equal(pretty_bytes(123456), "123.46 kB")
  expect_equal(pretty_bytes(1234567), "1.23 MB")
  expect_equal(pretty_bytes(12345678), "12.35 MB")
  expect_equal(pretty_bytes(123456789), "123.46 MB")
  expect_equal(pretty_bytes(1234567890), "1.23 GB")
  expect_equal(pretty_bytes(12345678901), "12.35 GB")
  expect_equal(pretty_bytes(123456789012), "123.46 GB")
})


test_that("set_attributes", {
  expect_identical(set_attributes(1L, NULL), 1L)
  expect_identical(set_attributes(1L, list(a = 2L)),
                   structure(1L, a = 2L))
})


test_that("has_colour", {
  expect_equal(has_colour(NULL), crayon::has_color())

  path <- tempfile_test()
  con <- file(path, "w")
  on.exit(close(con))
  expect_false(has_colour(con))
  close(con)
  on.exit()
  unlink(path)
})


test_that("data_frame", {
  a <- 1:10
  b <- sample(letters, 10)
  expect_equal(data_frame(a, b), data.frame(a, b, stringsAsFactors = FALSE))
})


test_that("nothing", {
  expect_equal(withVisible(nothing()),
               list(value = NULL, visible = FALSE))
})


test_that("read_binary", {
  p <- tempfile_test()
  on.exit(unlink(p))
  bytes <- as.raw(sample(0:255, 10000, replace = TRUE))
  writeBin(bytes, p)
  expect_identical(read_binary(p), bytes)
})


test_that("indent", {
  expect_equal(indent("a", 4), "    a")
  expect_equal(indent(letters, 2), paste0("  ", letters))
})


test_that("base64encode", {
  str <- "hello world"
  res <- "aGVsbG8gd29ybGQ="
  expect_identical(base64encode(str), res)
  expect_identical(base64decode(res), str)
})


test_that("base64encode - urlsafe", {
  d <- as.raw(c(0xf9, 0x9f, 0xe3, 0xb7, 0x93, 0x17, 0xcb, 0xc4, 0x53, 0x03))
  s1 <- "+Z/jt5MXy8RTAw=="
  s2 <- "-Z_jt5MXy8RTAw=="
  expect_equal(base64encode(d), s1)
  expect_equal(base64encode(d, TRUE), s2)

  expect_equal(base64decode(s1), rawToChar(d))
  expect_equal(base64decode(s2, TRUE), rawToChar(d))
})


test_that("sprintfn", {
  expect_equal(sprintfn("mystring", character(0)), "mystring")
  expect_equal(sprintfn("xx %s yy", "aa"), "xx aa yy")
  expect_equal(sprintfn("xx %s %s yy", c("aa", "bb")), "xx aa bb yy")
  expect_error(sprintfn("xx %s %s %s yy", c("aa", "bb", "cc")),
               "Not implemented [stevedore bug]", fixed = TRUE)
})


test_that("download_file", {
  skip_if_no_internet()
  dest <- download_file("https://google.com", tempfile_test(), TRUE)
  expect_true(file.exists(dest))
  writeLines("testing", dest)
  expect_identical(download_file("https://google.com", dest, TRUE), dest)
  expect_silent(download_file("https://google.com", dest, FALSE))
  expect_equal(readLines(dest), "testing")
  unlink(dest)
})


test_that("capture_args", {
  expect_identical(capture_args(function() 1, "foo"), "    foo()")
  expect_identical(capture_args(function() 1, "foo", 2), "  foo()")

  expect_identical(capture_args(function(a = 1) 1, "foo"), "    foo(a = 1)")
  expect_identical(capture_args(function(a = 1) 1, "foo", 2), "  foo(a = 1)")
  expect_identical(capture_args(function(a = 1L) 1, "foo"), "    foo(a = 1L)")

  expect_identical(capture_args(function(a) 1, "foo"), "    foo(a)")

  expect_identical(capture_args(
    function(very_long_first_arg = very_long_default_value) 1, "foo", 4, 20),
    "    foo(very_long_first_arg = very_long_default_value)")
  expect_identical(capture_args(
    function(very_long_first_arg = very_long_default_value, b = 1) 1, "foo",
    4, 20),
    "    foo(very_long_first_arg = very_long_default_value,\n        b = 1)")
  expect_identical(capture_args(
    function(very_long_first_arg = very_long_default_value, b = 1) 1, "foo",
    4, 1000),
    "    foo(very_long_first_arg = very_long_default_value, b = 1)")
})


test_that("join_text_list", {
  expect_identical(join_text_list(character()), character())
  expect_identical(join_text_list("a"), "a")
  expect_identical(join_text_list(c("a", "b")), "a and b")
  expect_identical(join_text_list(c("a", "b", "c")), "a, b and c")
})


test_that("pass_through", {
  expect_identical(pass_through(iris), iris)
})


test_that("parse_timestamp", {
  t <- parse_timestamp("2018-03-22T11:51:26.854401Z")
  expect_false(is.na(t))
  expect_is(t, "POSIXt")
  expect_is(t, "POSIXlt")

  cmp <- structure(list(sec = 26.854401, min = 51L, hour = 11L, mday = 22L,
                        mon = 2L, year = 118L, wday = 4L, yday = 80L,
                        isdst = 0L),
                   class = c("POSIXlt", "POSIXt"), tzone = "GMT")
  expect_identical(t, cmp)
})


test_that("time_ago", {
  timestamp <- "2018-03-22T11:51:26.854401Z"
  t <- parse_timestamp(timestamp)

  expect_equal(time_ago(timestamp, t), "0 secs ago")
  expect_equal(time_ago(timestamp, t + 20), "20 secs ago")
  expect_equal(time_ago(timestamp, t + 20 * 60), "20 mins ago")
  expect_equal(time_ago(timestamp, t + 60 * 60 * 3), "3 hours ago")
  expect_equal(time_ago(timestamp, t + 60 * 60 * 24 * 2), "2 days ago")
})


test_that("cat2", {
  expect_output(cat2("hello", file = stdout()), "hello")
  expect_silent(cat2("hello", file = NULL))
})


test_that("Sys_which", {
  expect_error(Sys_which(rand_str(32)),
               "Command '.{32}' not found on PATH")
})


test_that("system3", {
  skip_on_windows()

  expect_silent(res <- system3("id", "-adsfa"))
  expect_false(res$success)
  expect_error(system3("id", "-adsfa", check = TRUE),
               paste(res$output, collapse = "\n"), fixed = TRUE)
})
