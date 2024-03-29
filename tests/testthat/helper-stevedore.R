run_sample_responses <- function(obj) {
  for (x in obj$endpoints) {
    run_sample_response(x, obj$spec)
  }
}


run_sample_response <- function(x, spec) {
  responses <- spec$paths[[x$path]][[tolower(x$method)]]$responses
  message(sprintf("%s %s", toupper(x$method), x$path))
  ret <- vector("list", length(responses))
  names(ret) <- names(responses)
  for (code in names(responses)) {
    if (as.integer(code) < 300) {
      found_example <- FALSE
      response <- responses[[code]]
      response$schema <- resolve_schema_ref(response$schema, spec)
      ex <- response[["schema"]][["example"]]
      if (!is.null(ex)) {
        found_example <- TRUE
        message(crayon::blue("  - ", code))
        testthat::expect_silent(
          ret[[code]] <- x$response_handlers[[code]](ex, FALSE))
      }
      ex <- response[["examples"]]
      if (!is.null(ex)) {
        found_example <- TRUE
        message(crayon::blue("  - ", code))
        for (e in ex) {
          testthat::expect_silent(
            ret[[code]] <- x$response_handlers[[code]](e, FALSE))
        }
      }
      if (!found_example) {
        message(crayon::red("  - (no example)"))
      }
    }
  }
  invisible(ret)
}


describe_api <- function(x) {
  endpoints <- lapply(x$spec$paths, names)
  tag <- lapply(x$spec$paths, vcapply, function(el) el$tags %||% NA_character_)
  n <- lengths(endpoints)
  stopifnot(all(lengths(tag) == n))
  info <- data.frame(method = unname(unlist(endpoints, FALSE)),
                     path = rep(names(endpoints), n),
                     tag = unname(unlist(tag, FALSE)),
                     stringsAsFactors = FALSE)
  ours <- data.frame(method = vcapply(x$endpoints, "[[", "method",
                                      USE.NAMES = FALSE),
                     path = vcapply(x$endpoints, "[[", "path",
                                    USE.NAMES = FALSE),
                     stringsAsFactors = FALSE)
  p <- function(x) {
    paste(toupper(x$method), x$path, sep = " ")
  }
  info$included <- p(info) %in% p(ours)

  msg <- info[!info$included, ]
  msg <- split(p(msg), msg$tag)
  all <- split(p(info), info$tag)
  for (i in names(msg)) {
    message(sprintf("%s:", i))
    if (length(msg[[i]]) == length(all[[i]])) {
      message("  (none implemented)")
    } else {
      message(paste(sprintf("  - %s", msg[[i]]), collapse = "\n"))
    }
  }

  invisible(info)
}


read_sample_response <- function(path) {
  ret <- read_sample_response_header(path)
  spec <- swagger_spec_read(ret$version)

  endpoint <- spec$paths[[ret$path]][[ret$method]]
  ret$schema <- endpoint$responses[[ret$code]]

  ret$produces <- get_response_type(ret$method, ret$path, endpoint)

  ret$handler <- swagger_response_handler(ret$schema, spec, ret$produces)
  ret$reference <- eval(parse(text = ret$txt))

  ret
}


read_sample_response_header <- function(path) {
  txt <- readLines(path)

  response_json <- sub("\\.R$", ".json", path)
  if (file.exists(response_json)) {
    response <- readChar(response_json, file.size(response_json))
  } else {
    response <- NULL
  }
  ret <- parse_sample_response(txt, response)

  if (ret$response == "~") {
    ret$response <- raw()
  } else {
    ret$response <- charToRaw(ret$response)
  }

  ret$txt <- txt

  ret
}


parse_sample_response <- function(txt, response) {
  i <- grep("^[^#]", txt)[[1]]
  head <- sub("^#+\\s*", "", txt[seq_len(i - 1L)])
  re <- "(^[^ ]+): +(.*)\\s*$"
  stopifnot(all(grepl(re, head)))
  value <- sub(re, "\\2", head)
  ret <- set_names(as.list(value), sub(re, "\\1", head))

  if (!is.null(response)) {
    ret$response <- response
  }

  msg <- setdiff(c("version", "method", "path", "code", "response"), names(ret))
  if (length(msg) > 0L) {
    stop(sprintf("Missing expected fields %s", paste(msg, collapse = ", ")))
  }

  ret$method <- tolower(ret$method)
  ret
}


read_sample_response_str <- function(method, path, code, spec, error = TRUE) {
  r <- spec$paths[[path]][[tolower(method)]]$responses[[as.character(code)]]
  to_str <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)

  schema <- resolve_schema_ref(r$schema, spec)
  ex <- schema[["example"]]
  if (!is.null(ex)) {
    return(to_str(ex))
  }
  ex <- r[["examples"]]
  if (!is.null(ex)) {
    return(to_str(ex[[1]]))
  }
  ex <- r[["schema"]][["example"]]
  if (!is.null(ex)) {
    return(to_str(ex))
  }
  if (identical(r$schema$type, "array")) {
    r$schema$items <- resolve_schema_ref(r$schema$items, spec)
    ex1 <- r$schema$items$example
    if (!is.null(ex1)) {
      return(to_str(list(ex1)))
    }
  }
  msg <- sprintf("did not find example for %s %s", method, path)
  if (error) {
    stop(msg)
  } else {
    message(msg)
  }
  NULL
}


dput2 <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}


dput_list <- function(obj) {
  tmp <- vcapply(obj, dput2)
  els <- paste(sprintf("  %s = %s", names(tmp), unname(tmp)), collapse = ",\n")
  sprintf("list(\n%s\n  )", els)
}


dput_cvec <- function(x) {
  els <- paste(sprintf('  %s = "%s"', names(x), unname(x)), collapse = ",\n")
  sprintf("c(\n%s\n  )", els)
}


add_sample_response <- function(filename, method, path, code, version,
                                error = FALSE) {
  if (file.exists(filename)) {
    if (error) {
      stop("filename already exists")
    }
  }
  spec <- swagger_spec_read(version)
  response <- read_sample_response_str(method, path, code, spec, error)
  if (is.null(response)) {
    return(invisible())
  }
  dat <- list(version = version, method = method, path = path, code = code,
              response = response)
  txt <- c(sprintf("## %s: %s", names(dat), unname(dat)), "NULL")
  writeLines(txt, filename)
}


rand_str <- function(n, prefix = "") {
  paste0(prefix, paste0(sample(letters, n, replace = TRUE), collapse = ""))
}


random_hex <- function(n, prefix = "") {
  pool <- c(as.character(0:9), letters[1:6])
  paste0(prefix, paste0(sample(pool, n, replace = TRUE), collapse = ""))
}


get_error <- function(expr) {
  tryCatch(expr, error = identity)
}

## TODO: at some point a variant of this will move into the main
## constructor, with a proper error message advising on solutions.
## But I think that this is updated with curl version so could just
## come out surely?
CURL_HAS_SOCKET_SUPPORT <-
  !inherits(try(curl::curl_options()[["unix_socket_path"]], silent = TRUE),
            "try-error")
skip_if_no_curl_socket <- function() {
  if (!CURL_HAS_SOCKET_SUPPORT) {
    testthat::skip("libcurl does not have support for unix sockets")
  }
}


null_docker_client <- function(...) {
  docker_client(..., http_client_type = "null", ignore_environment = TRUE)
}


has_internet <- function() {
  !is.null(suppressWarnings(utils::nsl("www.google.com")))
}


skip_if_no_internet <- function() {
  skip_on_cran()
  skip_on_windows()
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
}


skip_if_no_httppipe_support <- function() {
  skip("httppipe needs update for new python/reticulate")
  skip_on_cran()
  if (!httppipe_available()) {
    testthat::skip("httppipe support not possible")
  }
}


test_sample_responses <- function(v, skip = NULL) {
  files <- dir(file.path("sample_responses", paste0("v", v)),
               pattern = "\\.R$", full.names = TRUE)

  for (file in files) {
    base <- sub("\\.R$", "", basename(file))
    testthat::test_that(sprintf("sample_responses: v%s - %s", v, base), {
      if (base %in% skip) {
        testthat::skip("not yet working")
      }
      dat <- read_sample_response(file)

      opts1 <- list(as_is_names = FALSE, data_frame = identity)
      opts2 <- list(as_is_names = TRUE, data_frame = identity)
      ans1 <- dat$handler(dat$response, opts1)
      ans2 <- dat$handler(dat$response, opts2)
      testthat::expect_equal(ans1, dat$reference)
      testthat::expect_equal(ans2, dat$reference, check.attributes = FALSE)
    })
  }

  res <- audit_spec_response(v)
  testthat::expect_false(any(res$missing))
}


create_sample_responses <- function(target, base) {
  path_base <- file.path("sample_responses", paste0("v", base))
  path_target <- file.path("sample_responses", paste0("v", target))
  spec <- swagger_spec_read(target)

  files <- dir(path_base, "\\.R$", full.names = TRUE)

  re_fmt <- "^(#+ %s:\\s*)(.+)\\s*$"

  f <- function(filename) {
    x <- readLines(filename)
    re_version <- sprintf(re_fmt, "version")
    i <- grep(re_version, x)
    stopifnot(length(i) == 1L)
    x[[i]] <- paste0(sub(re_version, "\\1", x[[i]]), target)

    response_json <- sub("\\.R$", ".json", filename)
    if (file.exists(response_json)) {
      response_prev <- readChar(response_json, file.size(response_json))
    } else {
      response_prev <- NULL
    }
    d <- parse_sample_response(x, response_prev)

    response <- read_sample_response_str(d$method, d$path, d$code, spec, FALSE)
    if (is.null(response)) {
      if (d$response == "~") {
        response <- "~"
      } else {
        response <- "<fill me in>"
      }
    }
    ## Special casing required for ping which is the only tested plain
    ## text endpoint.
    if (d$path == "/_ping") {
      response <- from_json(response)
    }

    re_response <- sprintf(re_fmt, "response")
    i <- grep(re_response, x)
    if (length(i) > 0L) {
      stopifnot(length(i) == 1L)
      x[[i]] <- paste0(sub(re_response, "\\1", x[[i]]), response)
    } else {
      re_code <- sprintf(re_fmt, "code")
      i <- grep(re_code, x)
      stopifnot(length(i) == 1L)
      x <- append(x, paste("## response:", response), i)
    }

    x
  }

  dir.create(path_target, FALSE, TRUE)
  for (filename in files) {
    dest <- file.path(path_target, basename(filename))
    if (file.exists(dest)) {
      message(sprintf("Destination %s exists - skipping", dest))
      next
    }
    res <- f(filename)
    if (!is.null(res)) {
      writeLines(res, dest)
    }
  }
}


repeat_until_error <- function(fn, times = 10L, interval = 0.1) {
  for (i in seq_len(times)) {
    e <- get_error(fn())
    if (is_error(e)) {
      return(e)
    } else {
      Sys.sleep(0.1)
    }
  }
  stop("Did not throw error in time")
}


fake_pager <- function(dest) {
  force(dest)
  function(files, header, title, delete.file) {
    file.copy(files, dest)
    if (delete.file) {
      unlink(files)
    }
  }
}


skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}


make_fake_files <- function(paths) {
  root <- tempfile_test()
  paths <- file.path(root, paths)
  for (d in unique(dirname(paths))) {
    dir.create(d, FALSE, TRUE)
  }
  for (p in grep("/$", paths, invert = TRUE, value = TRUE)) {
    file.create(p)
  }
  root
}


get_stevedorebot_pass <- function() {
  pw <- Sys.getenv("STEVEDORE_STEVEDOREBOT_PASS", "")
  if (!nzchar(pw)) {
    testthat::skip("'STEVEDORE_STEVEDOREBOT_PASS' is not defined")
  }
  pw
}


set_dummy_id <- function(value) {
  prev <- .stevedore$dummy_id
  .stevedore$dummy_id <- value
  invisible(prev)
}


update_dummy_attrs <- function(object, value) {
  object$.attrs[names(value)] <- value
  invisible(object)
}


update_name_cache <- function(root) {
  testthat::test_file(file.path(root, "tests/testthat/test-help.R"))

  names <- .stevedore$names
  names <- names[order(names[, 1L]), ]

  ex <- read.csv(file.path(root, "inst/spec/names_override.csv"),
                 stringsAsFactors = FALSE)
  i <- match(ex$from, names[, 1L])
  stopifnot(!any(is.na(i)))

  names[i, "to"] <- ex$to

  write.csv(names, file.path(root, "inst/spec/names.csv"), row.names = FALSE)

  invisible(names)
}


audit_spec_response <- function(v) {
  endpoints <- docker_api_client_endpoints()
  produces <- function(x, spec) {
    d <- spec$paths[[x$path]][[x$method]]
    type <- get_response_type(x$method, x$path, d)
    if (type == "application/json") {
      response <- d$responses[as.integer(names(d$responses)) < 300][[1]]
      if (is.null(resolve_schema_ref(response$schema, spec))) {
        type <- "null"
      }
    }
    type
  }
  spec <- swagger_spec_read(DOCKER_API_VERSION_MAX)
  produces <- vcapply(endpoints, produces, spec)

  files <- dir(sprintf("sample_responses/v%s", v), pattern = "\\.R$",
               full.names = TRUE)

  tmp <- lapply(files, read_sample_response_header)
  ok <- vcapply(tmp, "[[", "version") == v
  if (!all(ok)) {
    stop(sprintf("Response has wrong version: %s",
                 paste(files[!ok], collapse = ", ")))
  }

  p1 <- vcapply(endpoints, function(x) paste(tolower(x$method), x$path))
  p2 <- vcapply(tmp, function(x) paste(tolower(x$method), x$path))
  tested <- p1 %in% p2

  supported <-
    numeric_version(vcapply(endpoints, function(x) x$from %||% "0.0.0")) <= v

  ret <- data_frame(method = vcapply(endpoints, "[[", "method"),
                    path = vcapply(endpoints, "[[", "path"),
                    supported = supported,
                    produces = produces,
                    tested = tested)

  exclude <- c("post /build", "post /images/create", "post /images/load",
               "post /plugins/pull")
  ret$missing <-
    ret$supported & ret$produces == "application/json" & !ret$tested &
    !(paste(ret$method, ret$path) %in% exclude)

  ret
}


fake_tls_dir <- function() {
  path <- tempfile_test()
  dir.create(path)
  files <- c("key.pem", "ca.pem", "cert.pem")
  for (p in files) {
    file.create(file.path(path, p))
  }
  path
}


wait_until_ready <- function(f, times = 10, period = 0.5) {
  for (i in seq_len(times)) {
    dat <- tryCatch(f(), error = function(e) FALSE)
    if (isTRUE(dat)) {
      return(invisible())
    }
    Sys.sleep(period)
  }
  stop("Not ready in time")
}


try_silent <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}


wait_until_container_gone <- function(container) {
  f <- function() {
    is.null(tryCatch(container$status(), error = function(e) NULL))
  }
  wait_until_ready(f, 100, 0.1)
}


stop_service_and_wait_until_service_container_gone <- function(service) {
  tasks <- service$tasks()
  cl <- service$.parent
  containers <- lapply(tasks, function(x)
    tryCatch(cl$container$get(x$inspect()$status$container_status$container_id),
             error = function(e) NULL))
  service$remove()

  for (x in containers) {
    if (!is.null(x)) {
      wait_until_container_gone(x)
    }
  }
}


## Untar a raw vector
untar_bin <- function(bin, path = tempfile_test(), ...) {
  tmp <- tempfile_test()
  writeBin(bin, tmp)
  on.exit(unlink(tmp))
  dir.create(path, FALSE, TRUE)
  utils::untar(tmp, exdir = path, ...)
  invisible(path)
}


tempfile_test <- function(tmpdir = tempdir(), fileext = "") {
  tempfile("stevedore_test", tmpdir, fileext)
}


cleanup_tempdir <- function() {
  files <- dir(tempdir(), pattern = "^stevedore_test_", full.names = TRUE)
  unlink(files, recursive = TRUE)
}


fake_docker_machine <- function(env, path = tempfile()) {
  dir.create(path, FALSE, TRUE)
  dest <- file.path(path, "docker-machine")
  code <- c(
    "#!/usr/bin/env bash",
    sprintf('echo \'export %s="%s"\'', names(env), unname(env)))

  writeLines(code, dest)
  Sys.chmod(dest, "755")
  path
}


dummy_data_frame_wrapper <- function(x) {
  class(x) <- c("extra", class(x))
  x
}


skip_if_external_tar_unsupported <- function() {
  testthat::skip_on_os("solaris")
  testthat::skip_on_cran()
  if (tolower(Sys.info()[["sysname"]]) == "solaris") {
    testthat::skip("Stevedore does not support this tar")
  }
}


capture_output_no_crayon <- function(code) {
  withr::with_options(
    list(crayon.enabled = FALSE),
    capture.output(force(code)))
}
