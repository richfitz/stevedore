stevedore_test_preflight_status <- function(cl) {
  msg <- character()

  v <- cl$volume$list()$name
  n <- length(v)
  if (n > 0L) {
    msg <- c(msg, sprintf("%d docker %s: %s",
                          n, ngettext(n, "volume", "volumes"),
                          paste(v, collapse = ", ")))
  }
  v <- cl$container$list()$name
  n <- length(v)
  if (n > 0L) {
    msg <- c(msg, sprintf("%d docker %s: %s",
                          n, ngettext(n, "container", "containers"),
                          paste(v, collapse = ", ")))
  }

  v <- setdiff(cl$network$list()$name,
               c("host", "bridge", "none", "docker_gwbridge"))
  n <- length(v)
  if (n > 0L) {
    msg <- c(msg, sprintf("%d unexpected docker %s: %s",
                          n, ngettext(n, "network", "networks"),
                          paste(v, collapse = ", ")))
  }

  req_external <- c("hello-world:latest",
                    "alpine:latest",
                    "alpine:3.1",
                    "bfirsh/reticulate-splines:latest",
                    "nginx:latest")
  img <- cl$image$list()

  img_msg <- setdiff(req_external, unlist(img$repo_tags))
  if (length(img_msg) > 0L) {
    msg <- c(msg, sprintf("Missing required external %s: %s",
                          ngettext(length(img_msg), "image", "images"),
                          paste(img_msg, collapse = ", ")))
  }

  req_internal <- sprintf(
    "richfitz/%s:latest",
    list.dirs("images", recursive = FALSE, full.names = FALSE))
  img_msg <- setdiff(req_internal, unlist(img$repo_tags))
  if (length(img_msg) > 0L) {
    msg <- c(msg, sprintf("Missing required internal %s: %s",
                          ngettext(length(img_msg), "image", "images"),
                          paste(img_msg, collapse = ", ")))
  }

  if (length(msg) > 0L) {
    msg <- paste0("Preflight errors:\n",
                  paste("-", msg, collapse = "\n"))
  } else {
    msg <- NULL
  }
  list(failed = !is.null(msg), messages = msg)
}


stevedore_preflight <- function() {
  if (!identical(Sys.getenv("STEVEDORE_TEST_USE_DOCKER"), "true")) {
    return(list(
      use_docker = FALSE,
      reason = "docker using tests not enabled (STEVEDORE_TEST_USE_DOCKER)"))
  }

  cl <- tryCatch(docker_client(), error = function(e) e)
  if (inherits(cl, "error")) {
    return(list(
      use_docker = FALSE,
      reason = "creating docker client failed",
      error = cl))
  }

  res <- tryCatch(cl$ping(), error = function(e) e)
  if (inherits(cl, "error")) {
    return(list(
      use_docker = FALSE,
      reason = "communicating with the docker client failed",
      error = cl))
  }

  res <- stevedore_test_preflight_status(cl)
  if (res$failed) {
    if (identical(Sys.getenv("STEVEDORE_TEST_REQUIRE_DOCKER", ""), "true")) {
      stop(res$messages)
    }
    return(list(
      use_docker = FALSE,
      reason = res$messages))
  }

  v <- cl$version()
  v_min <- max(numeric_version(v$min_api_version),
               numeric_version(DOCKER_API_VERSION_MIN))
  v_max <- min(numeric_version(v$api_version),
               numeric_version(DOCKER_API_VERSION_MAX))
  versions <- version_range(v_min, v_max)

  machine <- Sys_getenv1("STEVEDORE_TEST_DOCKER_MACHINE_NAME")
  if (!is.null(machine)) {
    machine <- tryCatch(get_machine_env(machine), error = function(e) NULL)
  }

  list(use_docker = TRUE,
       versions = versions,
       machine = machine)
}


STEVEDORE_TEST_INFO <- NULL
stevedore_test_info <- function() {
  if (is.null(STEVEDORE_TEST_INFO)) {
    STEVEDORE_TEST_INFO <- stevedore_preflight()
  }
  STEVEDORE_TEST_INFO
}

skip_if_not_using_docker <- function() {
  if (!stevedore_test_info()$use_docker) {
    testthat::skip(stevedore_test_info()$reason)
  }
}

test_docker_client <- function(...) {
  skip_if_no_curl_socket()
  skip_if_not_using_docker()
  docker_client(..., ignore_environment = TRUE)
}


test_machine_info <- function() {
  skip_if_not_using_docker()
  if (is.null(stevedore_test_info()$machine)) {
    testthat::skip("docker-machine not enabled")
  }
  stevedore_test_info()$machine
}


test_docker_versions <- function() {
  stevedore_test_info()$versions
}
