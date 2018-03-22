##' Create a docker client object
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help()}
##'
##' @title Create docker client
##'
##' @param api_version Version of the API request from the api.
##'   Options are \code{NULL} (the default) - use the package's
##'   default version (currently
##'   \Sexpr{stevedore:::DEFAULT_DOCKER_API_VERSION}), a version as a
##'   string or \code{\link{numeric_version}} object (supported
##'   between \Sexpr{stevedore:::MIN_DOCKER_API_VERSION} and
##'   \Sexpr{stevedore:::MAX_DOCKER_API_VERSION}), or the string
##'   \code{detect} which will use the highest version out of the
##'   version reported by the api and
##'   \Sexpr{stevedore:::MAX_DOCKER_API_VERSION}.
##'
##' @param url The URL for the docker daemon.  This can be an absolute
##'   file path (for a unix socket on macOS/Linux), a named pipe
##'   (e.g., \code{npipe:////./pipe/docker_engine}) on Windows, or
##'   (eventually) an http or https url (e.g.,
##'   \code{https://localhost:8888}), though this is not yet
##'   supported.
##'
##' @param ... Reserved for future use.  Passing in any unrecognised
##'   argument will throw an error.
##'
##' @param http_client_type HTTP client type to use.  The options are
##'   (currently) "curl", which uses the \code{curl} package (works
##'   over unix sockets and eventually over TCP) and \code{httppipe}
##'   which works over unix sockets and eventually windows named
##'   pipes, using the Docker SDK's pipe code via the \code{httppipe}
##'   package.  Not all functionality is supported with the
##'   \code{httppipe} client.  This option may eventually be moved
##'   into the \code{...} argument as is not intended for end-user
##'   use; it is primarily intended for debugging in development
##'   (forcing the \code{httppipe} client where the \code{curl} client
##'   would ordinarily be preferred).
##'
##' @export
docker_client <- function(api_version = NULL, url = NULL, ...,
                          http_client_type = NULL) {
  assert_empty_dots(..., name = "docker_client")

  self <- new_stevedore_object(NULL)
  self$.api_client <-
    docker_api_client(base_url = url, api_version = api_version,
                      type = http_client_type, ...)
  self$types <- docker_client_types(self)

  self$events <- docker_client_method(
    "system_events", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$df <- docker_client_method("system_df", self)

  self$info <- docker_client_method("system_info", self)

  self$login <- docker_client_method(
    "system_auth", self,
    after = after_system_login)

  self$ping <- docker_client_method("system_ping", self)

  self$version <- docker_client_method("system_version", self)
  self$api_version <- function() self$.api_client$http_client$api_version

  self$containers <- docker_client_container_collection(self)
  self$images <- docker_client_image_collection(self)
  self$networks <- docker_client_network_collection(self)
  self$volumes <- docker_client_volume_collection(self)

  self$swarm <- docker_client_swarm_collection(self)
  self$nodes <- docker_client_node_collection(self)
  self$services <- docker_client_service_collection(self)
  self$tasks <- docker_client_task_collection(self)
  self$secrets <- docker_client_secret_collection(self)

  stevedore_object(self, "docker_client")
}

docker_client_container_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$run <- make_docker_run(parent)

  self$create <- docker_client_method(
    "container_create", self,
    promote = c("image", "cmd"),
    rename = c(ports = "exposed_ports",
               network = "networking_config",
               health_check = "healthcheck"),
    defaults = alist(image =),
    process = list(
      quote(image <- get_image_id(image)),
      quote(cmd <- validate_command(cmd)),
      quote(env <- validate_env(env)),
      mcr_volumes_for_create(quote(volumes), quote(host_config)),
      mcr_ports_for_create(quote(ports), quote(host_config)),
      mcr_network_for_create(quote(network), quote(host_config))),
    after = after_container_create)

  self$get <- docker_client_getter(docker_client_container, parent)

  self$list <- docker_client_method(
    "container_list", self,
    process = list(quote(filters <- as_docker_filter(filters))),
    after = after_container_list)

  self$remove <- docker_client_method(
    "container_delete", self,
    rename = c(delete_volumes = "v"),
    process = list(quote(id <- get_image_id(id))))

  self$prune <- docker_client_method(
    "container_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  stevedore_object(self, "docker_container_collection")
}

docker_client_container <- function(id, parent) {
  self <- new_stevedore_object(parent)
  fix_id <- docker_client_add_inspect(id, "id", "container_inspect", self)

  ## These will probably get rationalised soon as there is a pattern here
  self$name <- function() drop_leading_slash(self$inspect(FALSE)$name)
  self$labels <- function(reload = TRUE) self$inspect(reload)$config$labels
  self$status <- function(reload = TRUE) self$inspect(reload)$state$status
  self$image <- function() docker_client_container_image(self)
  self$ports <- function(reload = TRUE) {
    docker_client_container_ports(self$inspect(reload))
  }

  ## TODO: "attach" is hard because it might need to hijack the
  ## connection and deal with stdin (follow logs is close but not
  ## quite the same)
  ## attach = docker_client_method("container_attach", self,
  ##    fix = fix_id)
  ##
  ## NOTE: The promotion list for commit is to mimic the argument
  ## list for the command line version of `docker commit` (minus
  ## "id" which is fixed).
  self$commit <- docker_client_method(
    "image_commit", self,
    process = list(quote(env <- validate_env(env))),
    promote = c("repo", "tag", "author", "changes", "comment", "pause"),
    fix = list(container = id),
    after = after_image_commit)

  self$diff <- docker_client_method(
    "container_changes", self,
    fix = fix_id)

  ## TODO: inject 'start' into here too, which then requires passing
  ## detach through as well and dealing with those through the
  ## 'after' function.
  self$exec <- docker_client_method(
    "exec_create", self,
    fix = fix_id,
    rename = c(stdout = "attach_stdout",
               stderr = "attach_stderr",
               stdin = "attach_stdin"),
    defaults = alist(stdout = TRUE, stderr = TRUE, cmd =),
    promote = "cmd",
    process = list(quote(cmd <- validate_command(cmd)),
                   quote(env <- validate_env(env))),
    after = after_exec_create)

  self$export <- docker_client_method(
    "container_export", self,
    fix = fix_id)

  self$path_stat <- docker_client_method(
    "container_path_stat", self,
    fix = fix_id,
    after = after_container_path_stat)

  self$get_archive <- docker_client_method(
    "container_archive", self,
    fix = fix_id,
    extra = alist(dest =),
    process = list(quote(assert_scalar_character_or_null(dest))),
    after = after_container_archive)

  ## TODO: option for compression, pass through to tar file (much
  ## easier to get right if we can rely on R tar)
  self$put_archive <- docker_client_method(
    "container_import", self,
    fix = fix_id,
    rename = c(src = "input_stream"),
    process = list(quote(src <- validate_tar_input(src))),
    after = nothing)

  self$kill <- docker_client_method(
    "container_kill", self,
    fix = fix_id)

  ## Logs; quite complicated in the case of 'follow'
  ## -  stream has an effect *only* if follow is TRUE
  self$logs <- docker_client_method(
    "container_logs", self,
    fix = fix_id,
    defaults = list(stdout = TRUE, stderr = TRUE),
    process = list(
      quote(if (is.numeric(tail)) tail <- as.character(tail)),
      mcr_prepare_stream_and_close(quote(stream))),
    extra = alist(stream = stdout()),
    hijack = quote(
      if (isTRUE(follow)) streaming_text(docker_stream_printer(stream))),
    allow_hijack_without_stream = FALSE,
    after = after_container_logs)

  self$pause <- docker_client_method(
    "container_pause", self,
    fix = fix_id)

  self$remove <- docker_client_method(
    "container_delete", self,
    fix = fix_id,
    rename = c(delete_volumes = "v"))

  ## This might force refresh?
  self$rename <- docker_client_method(
    "container_rename", self,
    fix = fix_id)

  self$resize <- docker_client_method(
    "container_resize", self,
    fix = fix_id)

  self$restart <- docker_client_method(
    "container_restart", self,
    fix = fix_id)

  self$start <- docker_client_method(
    "container_start", self,
    fix = fix_id,
    after = invisible_self)

  ## TODO: expose stream (but with nice printing and escape instructions?)
  self$stats <- docker_client_method(
    "container_stats", self,
    fix = c(fix_id, stream = FALSE))

  self$stop <- docker_client_method(
    "container_stop", self,
    fix = fix_id)

  self$top <- docker_client_method(
    "container_top", self,
    fix = fix_id,
    after = after_container_top)

  self$unpause <- docker_client_method(
    "container_unpause", self,
    fix = fix_id)

  self$update <- docker_client_method(
    "container_update", self,
    fix = fix_id,
    after = after_container_update)

  self$wait <- docker_client_method(
    "container_wait", self,
    fix = fix_id)

  stevedore_object(self, "docker_container")
}


docker_client_image_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$get <- docker_client_getter(docker_client_image, parent)

  ## TODO: control returning output too
  self$build <- docker_client_method(
    "image_build", self,
    drop = "content_type",
    rename = c(context = "input_stream", tag = "t"),
    defaults = alist(context =),
    extra = alist(verbose = NULL, stream = stdout()),
    process = list(
      mcr_prepare_stream_and_close(quote(stream)),
      quote(context <- validate_tar_directory(context, dockerfile))),
    hijack = quote(streaming_json(build_status_printer(stream))),
    allow_hijack_without_stream = TRUE,
    after = after_image_build)

  self$list <- docker_client_method(
    "image_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$import <- docker_client_method("image_import", self)

  ## TODO: add filename argument for saving (see image_tarball)
  self$export <- docker_client_method("image_export", self)

  self$pull <- docker_client_method(
    "image_create", self,
    rename = c("name" = "from_image"),
    drop = c("input_image", "from_src", "repo", "registry_auth"),
    process = list(
      mcr_prepare_stream_and_close(quote(stream)),
      mcr_process_image_and_tag(quote(name), quote(tag)),
      mcr_prepare_auth(quote(name), quote(registry_auth))),
    extra = alist(stream = stdout()),
    defaults = alist(name =),
    hijack = quote(streaming_json(pull_status_printer(stream))),
    allow_hijack_without_stream = TRUE,
    after = after_image_pull)

  self$push <- docker_client_method(
    "image_push", self,
    drop = c("registry_auth", "tag"),
    extra = alist(stream = stdout()),
    hijack = quote(streaming_json(pull_status_printer(stream))),
    process = list(
      mcr_prepare_stream_and_close(quote(stream)),
      mcr_prepare_push(quote(name), quote(tag), quote(registry_auth))),
    after = after_image_push)

  self$search <- docker_client_method(
    "image_search", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method("image_delete", self)

  self$prune <- docker_client_method(
    "image_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$build_clean <- docker_client_method(
    "image_build_clean", self)

  stevedore_object(self, "docker_image_collection")
}

docker_client_image <- function(id, parent) {
  ## NOTE: used in 'name()' to record the given name - this is
  ## different to most of the other cases here and I'm not sure how
  ## incredibly useful this is.  But it's the only way that we get to
  ## record how the user requested the object vs all of the collected
  ## information attached to it.
  name <- id

  self <- new_stevedore_object(parent)
  fix_id_as_name <-
    docker_client_add_inspect(id, "id", "image_inspect", self, "name")

  self$name <- function() name
  self$labels <- function(reload = TRUE) self$inspect(reload)$config$labels
  self$short_id <- function() short_id(self$inspect(FALSE)$id)
  self$tags <- function(reload = TRUE) {
    docker_client_image_tags(self$inspect(reload))
  }

  self$history <- docker_client_method(
    "image_history", self,
    fix = fix_id_as_name)

  ## TODO: this needs to add a 'filename' option for saving
  self$export <- docker_client_method(
    "image_tarball", self,
    fix = fix_id_as_name)

  self$tag <- docker_client_method(
    "image_tag", self,
    fix = fix_id_as_name,
    after = invisible_self,
    defaults = alist(repo =))

  self$untag <- function(repo_tag) docker_client_image_untag(repo_tag, self)

  ## NOTE: this always tries to remove the image by *id* not by
  ## name, which is not ideal really.  When force = TRUE it's
  ## basically the same I think.
  self$remove <- docker_client_method(
    "image_delete", self,
    fix = fix_id_as_name)

  stevedore_object(self, "docker_image")
}

docker_client_network_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "network_create", self,
    after = after_network_create,
    defaults = alist(check_duplicate = TRUE))

  self$get <- docker_client_getter(docker_client_network, parent)

  self$list <- docker_client_method(
    "network_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method("network_delete", self)

  self$prune <- docker_client_method(
    "network_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  stevedore_object(self, "docker_network_collection")
}

docker_client_network <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "network_inspect", self)

  self$name <- function(reload = TRUE) self$inspect(reload)$name

  self$containers <- function(reload = TRUE) {
    docker_client_network_containers(reload, self)
  }

  self$connect <- docker_client_method(
    "network_connect", self,
    fix = fix_id)

  self$disconnect <- docker_client_method(
    "network_disconnect", self,
    fix = fix_id)

  self$remove <- docker_client_method(
    "network_delete", self,
    fix = fix_id)

  stevedore_object(self, "docker_network")
}

docker_client_volume_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "volume_create", self,
    after = after_volume_create)

  self$get <- docker_client_getter(docker_client_volume, parent, "name")

  self$list <- docker_client_method(
    "volume_list", self,
    after = after_volume_list,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method("volume_delete", self)

  self$prune <- docker_client_method(
    "volume_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  stevedore_object(self, "docker_volume_collection")
}

docker_client_volume <- function(name, parent) {
  self <- new_stevedore_object(parent)
  fix_name <- docker_client_add_inspect(name, "name", "volume_inspect", self)

  self$map <- function(path, readonly = FALSE) {
    docker_client_volume_map(self$inspect(FALSE), path, readonly)
  }

  self$remove <- docker_client_method(
    "volume_delete", self,
    fix = fix_name)

  stevedore_object(self, "docker_volume")
}

docker_client_exec <- function(id, parent) {
  self <- new_stevedore_object(parent)
  fix_id <- docker_client_add_inspect(id, "id", "exec_inspect", self)

  ## Even though it feels like there *should* be a way, there is no
  ## way to get back to a detached exec instance.
  ## https://github.com/moby/moby/issues/9527

  ## TODO: explicitly set 'detach' argument
  self$start <- docker_client_method(
    "exec_start", self,
    fix = fix_id,
    extra = alist(stream = stdout()),
    hijack = quote(streaming_text(docker_stream_printer(stream))),
    allow_hijack_without_stream = TRUE,
    process = list(mcr_prepare_stream_and_close(quote(stream))),
    after = after_exec_start)

  self$resize <- docker_client_method(
    "exec_resize", self,
    fix = fix_id)

  stevedore_object(self, "docker_exec")
}


docker_client_swarm_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$init <- docker_client_method(
    "swarm_init", self,
    defaults = alist(listen_addr = "0.0.0.0:2377"))
  self$inspect <- docker_client_method("swarm_inspect", self)
  self$join <- docker_client_method("swarm_join", self)
  self$leave <- docker_client_method("swarm_leave", self)
  self$update <- docker_client_method("swarm_update", self)
  self$unlock_key <- docker_client_method("swarm_unlock_key", self)
  self$unlock <- docker_client_method("swarm_unlock", self)

  stevedore_object(self, "docker_swarm_collection")
}


docker_client_node_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$inspect <- docker_client_method("node_inspect", self)
  self$list <- docker_client_method(
    "node_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  self$delete <- docker_client_method("node_delete", self)

  ## TODO: this one here needs a bunch of work actually - we need to
  ## automate the index but also allow for *partial* updates, perhaps.
  self$update <- docker_client_method("node_update", self)

  stevedore_object(self, "docker_swarm_collection")
}


docker_client_service_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "service_create", self,
    expand = c("task_template" = "task_spec",
               "container_spec" = "container_spec"),
    extra = alist(replicas = NULL, global = FALSE, detach = FALSE,
                  timeout = 60, time_wait_stable = 5, stream = stdout()),
    ## All the processing here is quite hard, but could be formalised
    ## - this is all processing of arguments that are in expanded
    ## types.  That's not a pattern used elsewhere yet.
    process = list(
      quote(assert_scalar_logical(detach)),
      quote(assert_scalar_numeric(timeout)),
      quote(assert_scalar_numeric(time_wait_stable)),
      mcr_prepare_stream_and_close(quote(stream)),
      quote(task_template <-
              validate_service_secrets(task_template, object$.parent)),
      quote(mode <- validate_service_replicas(replicas, global))),
    drop = "mode",
    after = after_service_create)

  self$get <- docker_client_getter(docker_client_service, parent, "id")

  self$list <- docker_client_method(
    "service_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method(
    "service_delete", self)

  stevedore_object(self, "docker_service_collection")
}


docker_client_service <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "service_inspect", self)

  self$name <- function(reload = TRUE) self$inspect(reload)$spec$name
  self$version <- function(reload = TRUE) self$inspect(reload)$version$index
  self$remove <- docker_client_method(
    "service_delete", self,
    fix = fix_id)
  self$tasks <- function(filters = NULL) {
    docker_client_service_tasks(self, filters)
  }
  self$ps <- function(resolve_names = TRUE, filters = NULL) {
    docker_client_service_ps(self, resolve_names, filters)
  }

  stevedore_object(self, "docker_service")
}


docker_client_task_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$list <- docker_client_method(
    "task_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  self$get <- docker_client_getter(docker_client_task, parent, "id")

  stevedore_object(self, "docker_task_collection")
}

docker_client_task <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "task_inspect", self)

  self$logs <- docker_client_method(
    "task_logs", self,
    fix = fix_id,
    defaults = list(stdout = TRUE, stderr = TRUE),
    process = list(
      quote(if (is.numeric(tail)) tail <- as.character(tail)),
      mcr_prepare_stream_and_close(quote(stream))),
    extra = alist(stream = stdout()),
    hijack = quote(
      if (isTRUE(follow)) streaming_text(docker_stream_printer(stream))),
    allow_hijack_without_stream = FALSE,
    after = after_task_logs)

  self$state <- function(reload = TRUE) self$inspect(reload)$status$state
  self$service <- function() {
    self$.parent$services$get(self$inspect(FALSE)$service_id)
  }

  stevedore_object(self, "docker_task")
}


docker_client_secret_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "secret_create", self,
    defaults = alist(name = , data =),
    promote = c("name", "data"),
    process = list(quote(data <- validate_secret_data(data))),
    after = after_secret_create)

  self$inspect <- docker_client_method(
    "secret_inspect", self)

  self$list <- docker_client_method(
    "secret_list", self,
    process = list(quote(filters <- as_docker_filter(filters))),
    after = after_secret_list)

  self$remove <- docker_client_method(
    "secret_delete", self)

  self$update <- docker_client_method(
    "secret_update", self)

  stevedore_object(self, "docker_secret_collection")
}


docker_client_types <- function(parent) {
  self <- new_stevedore_object(parent)
  types <- lapply(parent$.api_client$types, "[[", "reciever")
  list2env(types, self)
  stevedore_object(self, "docker_types")
}
