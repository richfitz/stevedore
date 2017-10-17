run_sample_responses <- function(obj) {
  for (x in obj$endpoints) {
    run_sample_response(x, obj$spec)
  }
}

run_sample_response <- function(x, spec) {
  responses <- spec$paths[[x$path]][[x$method]]$responses
  message(sprintf("%s %s", toupper(x$method), x$path))
  for (code in names(responses)) {
    if (as.integer(code) < 300) {
      found_example <- FALSE
      response <- responses[[code]]
      response$schema <- resolve_schema_ref(response$schema, spec)
      ex <- response[["schema"]][["example"]]
      if (!is.null(ex)) {
        found_example <- TRUE
        message(crayon::blue("  - ", code))
        expect_silent(x$response_handlers[[code]](ex, FALSE))
      }
      ex <- response[["examples"]]
      if (!is.null(ex)) {
        found_example <- TRUE
        message(crayon::blue("  - ", code))
        for (e in ex) {
          expect_silent(x$response_handlers[[code]](ex, FALSE))
        }
      }
      if (!found_example) {
        message(crayon::red("  - (no example)"))
      }
    }
  }
}
