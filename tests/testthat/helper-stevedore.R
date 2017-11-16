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
  i <- grep("^[^#]", txt)[[1]]
  head <- sub("^#+\\s*", "", txt[seq_len(i - 1L)])
  re <- "(^[^ ]+): +(.*)\\s*$"
  stopifnot(all(grepl(re, head)))
  value <- sub(re, "\\2", head)
  ret <- set_names(as.list(value), sub(re, "\\1", head))

  msg <- setdiff(names(ret), c("version", "method", "path", "code", "response"))
  if (length(msg) > 0L) {
    stop(sprintf("Missing expected fields %s", paste(msg, collapse = ", ")))
  }

  if (ret$response == "~") {
    ret$response <- raw()
  } else {
    ret$response <- charToRaw(ret$response)
  }

  ret$method <- tolower(ret$method)
  spec <- read_spec(ret$version)

  endpoint <- spec$paths[[ret$path]][[ret$method]]
  ret$schema <- endpoint$responses[[ret$code]]

  ret$produces <- get_response_type(ret$method, ret$path, endpoint)

  ret$handler <- make_response_handler(ret$schema, spec, ret$produces)
  ret$reference <- eval(parse(text = txt))

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
