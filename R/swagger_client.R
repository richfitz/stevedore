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
                            drop = NULL, defaults = NULL, promote = NULL,
                            after = NULL, hijack = NULL) {
  stopifnot(c("endpoints", "http_client") %in% names(client))
  endpoint <- client$endpoints[[name]]

  if (!is.null(hijack)) {
    assert_is(hijack, "call")
  }

  fenv <- new.env(parent = client, hash = FALSE)
  fenv$endpoint <- endpoint
  if (!is.null(after)) {
    fenv$after <- after
  }

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

  args_keep <- args[setdiff(names(args), c(names(fix), drop))]

  if (!is.null(defaults)) {
    stopifnot(all(names(defaults) %in% names(args_keep)))
    args_keep[names(defaults)] <- defaults
  }

  if (!is.null(promote)) {
    assert_character(promote, "character")
    stopifnot(all(promote %in% names(args_keep)))
    args_keep <- args_keep[c(promote, setdiff(names(args_keep), promote))]
  }

  subs <- list(
    name = name,
    hijack = hijack,
    get_params = as.call(c(list(quote(endpoint$argument_handler)),
                           lapply(names(args), as.name))))

  body <- substitute(expression({
    params <- get_params
    run_endpoint(http_client, endpoint, params, hijack = hijack)
  }), subs)[[2]]

  if (!is.null(after)) {
    n <- length(body)
    body[[n]] <- call("<-", quote(response), body[[n]])
    body[[n + 1L]] <- quote(after(response, params))
  }

  as.function(c(args_keep, body), fenv)
}
