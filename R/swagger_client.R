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

client_endpoint <- function(name, env) {
  args <- formals(env$endpoints[[name]]$argument_handler)
  subs <- list(
    name = name,
    get_params = as.call(c(list(quote(endpoint$argument_handler)),
                           lapply(names(args), as.name))))
  body <- substitute(expression({
    endpoint <- endpoints$name
    params <- get_params
    run_endpoint(client, endpoint, params, pass_error, hijack, as_is_names)
  }), subs)[[2]]
  base_args <- alist(pass_error = FALSE, hijack = FALSE, as_is_names = FALSE)
  as.function(c(args, base_args, body), env)
}

client_endpoints <- function(client, endpoints) {
  env <- new.env(parent = baseenv())
  env$client <- client
  env$endpoints <- endpoints
  ## We can either move this over or set things so that the parent
  ## environment is namespace:stevedore
  env$run_endpoint <- run_endpoint
  lock_environment(env)
  nms <- names(endpoints)
  set_names(lapply(names(endpoints), client_endpoint, env), nms)
}

stevedore_read_endpoints <- function() {
  path <- system.file("spec/endpoints.yaml", package = "stevedore",
                      mustWork = TRUE)
  yaml::yaml.load_file(path)
}
