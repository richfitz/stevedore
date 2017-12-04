docker_client_data <- function(version) {
  if (!(version %in% names(.stevedore$client_data))) {
    spec <- read_spec(version)
    .stevedore$client_data[[version]] <-
      list(spec = spec,
           endpoints = lapply(.stevedore$endpoints, function(x)
             make_endpoint(x$method, x$path, spec)))
  }
  .stevedore$client_data[[version]]
}

stevedore_read_endpoints <- function() {
  path <- system.file("spec/endpoints.yaml", package = "stevedore",
                      mustWork = TRUE)
  yaml::yaml.load_file(path)
}

docker_client_base <- function(..., api_version = NULL) {
  base_url <- NULL
  self <- new.env(parent = parent.env(environment()))
  self$http_client <- R6_http_client$new(base_url, api_version)
  dat <- suppressMessages(docker_client_data(self$http_client$api_version))
  self$endpoints <- dat$endpoints
  lock_environment(self)
  self
}

docker_endpoint <- function(name, client, fix = NULL, rename = NULL,
                            drop = NULL, defaults = NULL, extra = NULL,
                            promote = NULL, process = NULL, after = NULL,
                            hijack = NULL) {
  stopifnot(c("endpoints", "http_client") %in% names(client))
  endpoint <- client$endpoints[[name]]

  fenv <- new.env(parent = client, hash = FALSE)
  fenv$endpoint <- endpoint
  if (!is.null(fix)) {
    list2env(fix, fenv)
  }

  args <- formals(endpoint$argument_handler)

  if (!is.null(rename)) {
    assert_is(rename, "character")
    stopifnot(all(unname(rename) %in% names(args)),
              !any(names(rename) %in% names(args)))
    i <- match(rename, names(args))
    names(args)[i] <- names(rename)
  }

  if (!is.null(drop)) {
    assert_is(drop, "character")
    stopifnot(all(drop %in% names(args)))
    list2env(args[drop], fenv)
  }

  args_use <- args[setdiff(names(args), c(names(fix), drop))]

  if (!is.null(defaults)) {
    stopifnot(all(names(defaults) %in% names(args_use)))
    args_use[names(defaults)] <- defaults
  }

  if (!is.null(extra)) {
    args_use <- c(args_use, extra)
  }

  if (!is.null(promote)) {
    assert_character(promote, "character")
    stopifnot(all(promote %in% names(args_use)))
    args_use <- args_use[c(promote, setdiff(names(args_use), promote))]
  }

  if (!is.null(process)) {
    stopifnot(vlapply(process, is.language))
  }

  if (!is.null(hijack)) {
    assert_is(hijack, "call")
  }

  get_params <- as.call(c(list(quote(endpoint$argument_handler)),
                          lapply(names(args), as.name)))
  run_endpoint <- substitute(
    run_endpoint(http_client, endpoint, params, hijack = hijack),
    list(hijack = hijack))
  if (is.null(extra)) {
    add_extra <- NULL
  } else {
    add_extra <- bquote(
      params[.(as.call(c(quote(c), names(extra))))] <-
        .(as.call(c(quote(list), lapply(names(extra), as.name)))))
  }

  if (!is.null(after)) {
    fenv$after <- after
    finish <- c(call("<-", quote(response), run_endpoint),
                add_extra,
                quote(after(response, params)))
  } else {
    finish <- run_endpoint
  }

  body <- c(quote(`{`),
            unname(process),
            list(call("<-", quote(params), get_params)),
            finish)

  as.function(c(args_use, as.call(body)), fenv)
}
