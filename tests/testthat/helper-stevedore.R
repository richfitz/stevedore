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
  txt <- readLines(path)
  ret <- parse_sample_response(txt)

  if (ret$response == "~") {
    ret$response <- raw()
  } else {
    ret$response <- charToRaw(ret$response)
  }

  spec <- read_spec(ret$version)

  endpoint <- spec$paths[[ret$path]][[ret$method]]
  ret$schema <- endpoint$responses[[ret$code]]

  ret$produces <- get_response_type(ret$method, ret$path, endpoint)

  ret$handler <- make_response_handler(ret$schema, spec, ret$produces)
  ret$reference <- eval(parse(text = txt))

  ret
}

parse_sample_response <- function(txt) {
  i <- grep("^[^#]", txt)[[1]]
  head <- sub("^#+\\s*", "", txt[seq_len(i - 1L)])
  re <- "(^[^ ]+): +(.*)\\s*$"
  stopifnot(all(grepl(re, head)))
  value <- sub(re, "\\2", head)
  ret <- set_names(as.list(value), sub(re, "\\1", head))

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
  if (error) {
    stop("did not find example")
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

add_sample_response <- function(filename, method, path, code, version) {
  if (file.exists(filename)) {
    stop("filename already exists")
  }
  spec <- read_spec(version)
  response <- read_sample_response_str(method, path, code, spec)
  dat <- list(version = version, method = method, path = path, code = code,
              response = response)
  txt <- c(sprintf("## %s: %s", names(dat), unname(dat)), "NULL")
  writeLines(txt, filename)
}

rand_str <- function(n, prefix = "") {
  paste0(prefix, paste0(sample(letters, n, replace = TRUE), collapse = ""))
}

get_error <- function(expr) {
  tryCatch(expr, error = identity)
}

untar_bin <- function(bin, path = tempfile()) {
  tmp <- tempfile()
  writeBin(bin, tmp)
  on.exit(file.remove(tmp))
  dir.create(path, FALSE, TRUE)
  untar(tmp, exdir = path)
  invisible(path)
}

## TODO: at some point a variant of this will move into the main
## constructor, with a proper error message advising on solutions.
CURL_HAS_SOCKET_SUPPORT <-
  !inherits(try(curl::curl_options()[["unix_socket_path"]], silent = TRUE),
            "try-error")
skip_if_no_curl_socket <- function() {
  if (!CURL_HAS_SOCKET_SUPPORT) {
    testthat::skip("libcurl does not have support for unix sockets")
  }
}

test_docker_client <- function(...) {
  skip_if_no_curl_socket()
  ## TODO: stop here if connection fails too
  docker_client(...)
}

has_internet <- function() {
  !is.null(suppressWarnings(utils::nsl("www.google.com")))
}

skip_if_no_internet <- function() {
  if (has_internet()) {
    return()
  }
  testthat::skip("no internet")
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
      ans1 <- dat$handler(dat$response, FALSE)
      ans2 <- dat$handler(dat$response, TRUE)
      testthat::expect_equal(ans1, dat$reference)
      testthat::expect_equal(ans2, dat$reference, check.attributes = FALSE)
    })
  }
}

create_sample_responses <- function(target, base) {
  path_base <- file.path("sample_responses", paste0("v", base))
  path_target <- file.path("sample_responses", paste0("v", target))
  spec <- read_spec(target)

  files <- dir(path_base, full.names = TRUE)

  re_fmt <- "^(#+ %s:\\s*)(.+)\\s*$"

  f <- function(x) {
    x <- readLines(filename)
    re_version <- sprintf(re_fmt, "version")
    i <- grep(re_version, x)
    stopifnot(length(i) == 1L)
    x[[i]] <- paste0(sub(re_version, "\\1", x[[i]]), target)

    ## Then the response itself
    re_response <- sprintf(re_fmt, "response")
    i <- grep(re_response, x)
    stopifnot(length(i) == 1L)
    prev <- sub(re_response, "\\2", x[[i]])

    d <- parse_sample_response(x)
    response <- read_sample_response_str(d$method, d$path, d$code, spec, FALSE)
    if (is.null(response)) {
      if (d$response == "~") {
        response <- "~"
      } else {
        message("No response for ", basename(filename))
        return(NULL)
      }
    }
    ## Special casing required for ping which is the only tested plain
    ## text endpoint.
    if (d$path == "/_ping") {
      response <- from_json(response)
    }

    x[[i]] <- paste0(sub(re_response, "\\1", x[[i]]), response)

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
