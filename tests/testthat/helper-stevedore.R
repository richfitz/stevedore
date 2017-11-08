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
      response$schema <- resolve_schema_ref2(response$schema, spec)
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
