##' Create a docker client object, which allows you to interact with
##' docker from R.  The object has several \emph{methods} that allow
##' interaction with the docker daemon (for this object they are all
##' "system" commands) and \emph{collections}, which contains further
##' methods.  The client is structured similarly to the docker command
##' line client, such that \code{docker container create <args>} in
##' the command line becomes \code{docker$container$create(...)} in R
##' (if the client is called R).
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help()}
##'
##' @section Connection options:
##'
##' \code{stevedore} can connect to the docker daemon via a unix
##' socket (this is the default set-up on Linux and macOS), over a
##' named pipe (Windows 10 - see below) and https over a normal tcp
##' connection (this is especially useful with
##' \href{https://docs.docker.com/machine/}{\code{docker-machine}}.
##'
##' \enumerate{
##'
##' \item If the \code{machine} argument is given then
##'   \code{stevedore} queries \code{docker-machine} for settings.  If
##'   that command fails (e.g., there is no machine,
##'   \code{docker-machine} not installed) then that will cause an
##'   error.  (Note that the \code{docker-machine} output does not
##'   include API version information so the \code{api_version}
##'   argument is relevant, but \code{host}, \code{cert_path} and
##'   \code{tls_verify} will be silently ignored if provided).
##'
##' \item The arguments \code{host} overrides the environment variable
##'   \code{DOCKER_HOST}, \code{cert_path} overrides
##'   \code{DOCKER_CERT_PATH} and \code{tls_verify} overrides
##'   \code{DOCKER_TLS_VERIFY}.  If \code{ignore_environment} is
##'   \code{TRUE} then the environment variables are not used at all.
##'
##' \item if \code{code} is not provided by any of the above methods
##'   (machine, argument or environment variable) it will fall back
##'   on the default unix socket (\code{var/run/docker.sock}) on
##'   Linux/macOS or the default windows named pipe
##'   (\code{npipe:////./pipe/docker_engine}) on windows.
##'
##' }
##'
##' The API version is set by the \code{api_version} argument, which
##' falls back on the environment variable \code{DOCKER_API_VERSION}
##' (this is the same as the docker command line client and the python
##' SDK).  If neither are provided then \code{stevedore} will detect
##' the API version being used by the daemon and match that (provided
##' it falls within the range of versions supported by the package).
##'
##' @title Create docker client
##'
##' @param ... Reserved for future use.  Passing in any unrecognised
##'   argument will throw an error.  Part of the role of this argument
##'   is to force use of named arguments until the API is stabilised.
##'
##' @param api_version Version of the API to use when communicating
##'   with the docker daemon.  The default value, \code{NULL}, detects
##'   the docker server API version and attempts to match it (this
##'   mirrors the default behaviour of the docker command line
##'   client).  Alternatively, provide an API version number as a
##'   string or \code{\link{numeric_version}} object (supported
##'   between \Sexpr{stevedore:::DOCKER_API_VERSION_MIN} and
##'   \Sexpr{stevedore:::DOCKER_API_VERSION_MAX}).  The version
##'   \Sexpr{stevedore:::DOCKER_API_VERSION_DEFAULT} is the version
##'   used in most automated tests, and if problems are encountered,
##'   consider forcing this version).
##'
##' @param host The URL for the docker daemon.  This can be a unix
##'   socket (e.g., unix:///var/run/docker.sock) on macOS/Linux, a
##'   named pipe (e.g., \code{npipe:////./pipe/docker_engine}) on
##'   Windows, or an http or https url (e.g.,
##'   \code{https://localhost:2376}).  If not given, we use the
##'   environment variable \code{DOCKER_HOST}, falling back on the
##'   default socket or named pipe (for macOS/unix and windows
##'   respectively).
##'
##' @param cert_path The path to a directory containing certificate
##'   files.  If using an \code{https} url this is required.  If not
##'   given, we use the environment variable \code{DOCKER_CERT_PATH}.
##'   This is ignored without warning if used with a socket or named
##'   pipe connection.
##'
##' @param tls_verify Logical, indicating if TLS should be verified.
##'   This is only used if using an https connection (i.e., host is a
##'   tcp/http/https url and\code{cert_path} is given).  If not given,
##'   we use the environment variable \code{DOCKER_TLS_VERIFY}.
##'
##' @param machine Scalar character (if provided) indicating the name
##'   of a "docker machine" instance to use.  If this is provided then
##'   \code{docker-machine} must be installed and the machine must
##'   exist and be running.  \code{stevedore} will run
##'   \code{docker-machine env machine} to determine the environment
##'   variables to contact this machine and use these values for
##'   \code{host}, \code{cert_path} and \code{tls_verify} (silently
##'   ignoring any provided values).  Carl Boettiger is working on a
##'   \href{https://github.com/cboettig/dockermachine}{docker machine}
##'   package for R that would make managing docker machines from R
##'   easier.  As an alternative to this option, one can set
##'   docker-machine environment variables as described in
##'   \code{docker-machine env} before running R and they would be picked
##'   up as described above.
##'
##' @param http_client_type HTTP client type to use.  The options are
##'   (currently) "curl", which uses the \code{curl} package (works
##'   over unix sockets and over TCP) and \code{httppipe} which works
##'   over unix sockets and windows named pipes, using the Docker
##'   SDK's pipe code via the \code{httppipe} package.  Not all
##'   functionality is supported with the \code{httppipe} client.
##'   This option may eventually be moved into the \code{...} argument
##'   as is not intended for end-user use; it is primarily intended
##'   for debugging in development (forcing the \code{httppipe} client
##'   where the \code{curl} client would ordinarily be preferred).
##'
##' @param data_frame Function, used to wrap data.frames returned.
##'   This may make output easier to consume.  You might use
##'   \code{tibble::as_tibble} to return a \code{tbl_df} or
##'   \code{datatable::as.data.table} to return \code{data.table}
##'   objects.  This will be applied to all data.frames \emph{after}
##'   they are constructed, and so must take a single argument (the
##'   newly constructed data.frame) and return a new object that is
##'   largely compatible with data.frame.  Another use for this would
##'   be to define a function \code{data_frame = function(x)
##'   structure(x, class = c("foo", "data.frame"))} to set the class
##'   of all returned data.frame objects to be "foo" as well and then
##'   defining a custom S3 print method for "foo" that limited the
##'   output.
##'
##' @param ignore_environment Logical, indicating if environment
##'   variables (\code{DOCKER_HOST}, \code{DOCKER_CERT_PATH},
##'   \code{DOCKER_TLS_VERIFY} and \code{DOCKER_API_VERSION}) should
##'   be ignored (this has no effect if \code{machine} is specified).
##'
##' @param quiet Suppress informational messages.
##'
##' @param debug Enable http debugging (supported by the curl http
##'   driver only).  Provide a connection object and http headers and
##'   content will be sent to it.  Using \code{debug = TRUE} is
##'   equivalent to \code{code = stdout()}, while \code{debug = FALSE}
##'   is equivalent to \code{debug = NULL} (the default) which
##'   prevents debugging information being printed.  This option can
##'   be used to write to a file by opening a writeable connection but
##'   care must be made not to close this connection because otherwise
##'   the curl requests may fail.
##'
##' @export
##' @examples
##' if (stevedore::docker_available()) {
##'   # Create a new client object:
##'   client <- stevedore::docker_client()
##'
##'   # Version information for your docker daemon:
##'   client$version()
##'
##'   # General information about your daemon:
##'   client$info()
##'
##'   # Most of the interesting methods are within the collections.
##'   # For example, to see a summary of running containers:
##'   client$container$list()
##'
##'   # (see ?docker_container) for more information.
##' }
docker_client <- function(..., api_version = NULL,
                          host = NULL, cert_path = NULL, tls_verify = NULL,
                          machine = NULL,
                          http_client_type = NULL,
                          data_frame = NULL,
                          quiet = FALSE, debug = NULL,
                          ignore_environment = FALSE) {
  assert_empty_dots(..., name = "docker_client")

  config <- docker_config(api_version, host, cert_path, tls_verify, machine,
                          http_client_type = http_client_type,
                          data_frame = data_frame,
                          quiet = quiet, debug = debug,
                          ignore_environment = ignore_environment)

  self <- new_stevedore_object(NULL)
  self$.api_client <- docker_api_client(config)
  self$types <- docker_types(self)

  self$connection_info <- function() {
    connection_info(self$.api_client$http_client)
  }

  self$events <- docker_client_method(
    "system_events", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$df <- docker_client_method("system_df", self)

  self$info <- docker_client_method("system_info", self)

  self$login <- docker_client_method(
    "system_auth", self,
    after = after_system_login)

  self$ping <- docker_client_method("system_ping", self)

  self$cp <- function(src, dest) {
    docker_client_cp(self, src, dest)
  }

  self$version <- docker_client_method("system_version", self)
  self$api_version <- function() self$.api_client$http_client$api_version
  self$request <- make_docker_client_request(self)

  self$container <- docker_container_collection(self)
  self$image <- docker_image_collection(self)
  self$network <- docker_network_collection(self)
  self$volume <- docker_volume_collection(self)

  self$swarm <- docker_swarm_collection(self)
  self$node <- docker_node_collection(self)
  self$service <- docker_service_collection(self)
  self$task <- docker_task_collection(self)
  self$secret <- docker_secret_collection(self)
  self$config <- docker_config_collection(self)

  self$plugin <- docker_plugin_collection(self)

  stevedore_object(self, "docker_client",
                   "Control the docker daemon")
}


docker_container_collection <- function(parent) {
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
      quote(if (!is.null(network)) network <- get_network_id(network)),
      quote(env <- validate_env(env)),
      mcr_volumes_for_create(quote(volumes), quote(host_config)),
      mcr_ports_for_create(quote(ports), quote(host_config)),
      mcr_network_for_create(quote(network), quote(host_config))),
    after = after_container_create)

  self$get <- docker_client_getter(docker_container, parent)

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

  stevedore_object(self, "docker_container_collection",
                   "Work with docker containers")
}


docker_container <- function(id, parent) {
  self <- new_stevedore_object(parent)
  fix_id <- docker_client_add_inspect(id, "id", "container_inspect", self)

  ## These will probably get rationalised soon as there is a pattern here
  self$name <- function() drop_leading_slash(self$inspect(FALSE)$name)
  self$labels <- function(reload = TRUE) self$inspect(reload)$config$labels
  self$status <- function(reload = TRUE) self$inspect(reload)$state$status
  self$image <- function() docker_container_image(self)
  self$ports <- function(reload = TRUE) {
    docker_container_ports(self$inspect(reload), client_output_options(self))
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

  self$exec_create <- docker_client_method(
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

  self$exec <- make_container_exec(self)

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
    process = list(quote(assert_scalar_character_or_null(dest))),
    after = after_container_archive)

  self$put_archive <- docker_client_method(
    "container_import", self,
    fix = fix_id,
    rename = c(src = "input_stream"),
    process = list(quote(src <- validate_tar_input(src))),
    after = nothing)

  ## Two functions to around which docker cp can be built
  self$cp_in <- function(src, dest) docker_container_cp_in(self, src, dest)
  self$cp_out <- function(src, dest) docker_container_cp_out(self, src, dest)

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
    fix = fix_id,
    after = after_container_wait)

  stevedore_object(self, "docker_container",
                   "Work with a particular docker container")
}


docker_image_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$get <- docker_client_getter(docker_image, parent)

  ## TODO: control returning output too
  self$build <- docker_client_method(
    "image_build", self,
    drop = "content_type",
    rename = c(context = "input_stream", tag = "t"),
    defaults = alist(context =),
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
    defaults = alist(name =),
    hijack = quote(streaming_json(pull_status_printer(stream))),
    allow_hijack_without_stream = TRUE,
    after = after_image_pull)

  self$push <- docker_client_method(
    "image_push", self,
    drop = c("registry_auth", "tag"),
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

  stevedore_object(self, "docker_image_collection",
                   "Work with docker images")
}


docker_image <- function(id, parent) {
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
    docker_image_tags(self$inspect(reload))
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

  self$untag <- function(repo_tag) docker_image_untag(repo_tag, self)

  ## NOTE: this always tries to remove the image by *id* not by
  ## name, which is not ideal really.  When force = TRUE it's
  ## basically the same I think.
  self$remove <- docker_client_method(
    "image_delete", self,
    fix = fix_id_as_name)

  stevedore_object(self, "docker_image",
                   "Work with a particular docker image")
}


docker_network_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "network_create", self,
    after = after_network_create,
    defaults = alist(check_duplicate = TRUE))

  self$get <- docker_client_getter(docker_network, parent)

  self$list <- docker_client_method(
    "network_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method("network_delete", self)

  self$prune <- docker_client_method(
    "network_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  stevedore_object(self, "docker_network_collection",
                   "Work with docker networks")
}


docker_network <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "network_inspect", self)

  self$name <- function(reload = TRUE) self$inspect(reload)$name

  self$containers <- function(reload = TRUE) {
    docker_network_containers(reload, self)
  }

  self$connect <- docker_client_method(
    "network_connect", self,
    process = list(
      quote(container <- get_container_id(container))),
    fix = fix_id)

  self$disconnect <- docker_client_method(
    "network_disconnect", self,
    fix = fix_id)

  self$remove <- docker_client_method(
    "network_delete", self,
    fix = fix_id)

  stevedore_object(self, "docker_network",
                   "Work with a particular docker network")
}


docker_volume_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "volume_create", self,
    after = after_volume_create)

  self$get <- docker_client_getter(docker_volume, parent, "name")

  self$list <- docker_client_method(
    "volume_list", self,
    after = after_volume_list,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method("volume_delete", self)

  self$prune <- docker_client_method(
    "volume_prune", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  stevedore_object(self, "docker_volume_collection",
                   "Work with docker volumes")
}


docker_volume <- function(name, parent) {
  self <- new_stevedore_object(parent)
  fix_name <- docker_client_add_inspect(name, "name", "volume_inspect", self)

  self$map <- function(path, readonly = FALSE) {
    docker_volume_map(self$inspect(FALSE), path, readonly)
  }

  self$remove <- docker_client_method(
    "volume_delete", self,
    fix = fix_name)

  stevedore_object(self, "docker_volume",
                   "Work with a particular docker volume")
}


docker_exec <- function(id, parent) {
  self <- new_stevedore_object(parent)
  fix_id <- docker_client_add_inspect(id, "id", "exec_inspect", self)

  ## Even though it feels like there *should* be a way, there is no
  ## way to get back to a detached exec instance.
  ## https://github.com/moby/moby/issues/9527

  ## TODO: explicitly set 'detach' argument
  self$start <- docker_client_method(
    "exec_start", self,
    defaults = alist(detach = FALSE),
    fix = fix_id,
    hijack = quote(streaming_text(docker_stream_printer(stream))),
    allow_hijack_without_stream = TRUE,
    process = list(mcr_prepare_stream_and_close(quote(stream))),
    after = after_exec_start)

  self$resize <- docker_client_method(
    "exec_resize", self,
    fix = fix_id)

  stevedore_object(self, "docker_exec",
                   "Work with an 'exec' instance")
}


docker_swarm_collection <- function(parent) {
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

  stevedore_object(self, "docker_swarm_collection",
                   "Manage the docker swarm")
}


docker_node_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$get <- docker_client_getter(docker_node, parent, "id")

  self$list <- docker_client_method(
    "node_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  self$delete <- docker_client_method("node_delete", self)

  stevedore_object(self, "docker_node_collection",
                   "Manage docker swarm nodes")
}


docker_node <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "node_inspect", self)

  ## TODO: this one here needs a bunch of work actually - we need to
  ## automate the index but also allow for *partial* updates, perhaps.
  self$update <- docker_client_method(
    "node_update", self,
    fix = fix_id)

  self$hostname <- function(reload = TRUE) {
    self$inspect(reload)$description$hostname
  }
  self$version <- function(reload = TRUE) self$inspect(reload)$version$index
  self$status <- function(reload = TRUE) self$inspect(reload)$status$state
  self$role <- function(reload = TRUE) self$inspect(reload)$spec$role
  self$availability <- function(reload = TRUE) {
    self$inspect(reload)$spec$availability
  }

  stevedore_object(self, "docker_node",
                   "Work with a particular docker node")
}


docker_service_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "service_create", self,
    expand = c("task_template" = "task_spec",
               "container_spec" = "container_spec"),
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
      quote(task_template <-
              validate_service_configs(task_template, object$.parent)),
      quote(mode <- validate_service_replicas(replicas, global))),
    drop = "mode",
    after = after_service_create)

  self$get <- docker_client_getter(docker_service, parent, "id")

  self$list <- docker_client_method(
    "service_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))

  self$remove <- docker_client_method(
    "service_delete", self)

  stevedore_object(self, "docker_service_collection",
                   "Work with docker services")
}


docker_service <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "service_inspect", self)

  self$name <- function(reload = TRUE) self$inspect(reload)$spec$name
  self$version <- function(reload = TRUE) self$inspect(reload)$version$index
  self$remove <- docker_client_method(
    "service_delete", self,
    fix = fix_id)
  self$tasks <- function(filters = NULL) {
    docker_service_tasks(self, filters)
  }
  self$ps <- function(resolve_names = TRUE, filters = NULL) {
    docker_service_ps(self, resolve_names, filters)
  }

  stevedore_object(self, "docker_service",
                   "Work with a particular docker service")
}


docker_task_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$list <- docker_client_method(
    "task_list", self,
    process = list(quote(filters <- as_docker_filter(filters))))
  self$get <- docker_client_getter(docker_task, parent, "id")

  stevedore_object(self, "docker_task_collection",
                   "Work with docker tasks")
}


docker_task <- function(id, parent) {
  self <- new_stevedore_object(parent)

  fix_id <- docker_client_add_inspect(id, "id", "task_inspect", self)

  self$logs <- docker_client_method(
    "task_logs", self,
    fix = fix_id,
    defaults = list(stdout = TRUE, stderr = TRUE),
    process = list(
      quote(if (is.numeric(tail)) tail <- as.character(tail)),
      mcr_prepare_stream_and_close(quote(stream))),
    hijack = quote(
      if (isTRUE(follow)) streaming_text(docker_stream_printer(stream))),
    allow_hijack_without_stream = FALSE,
    after = after_task_logs)

  self$state <- function(reload = TRUE) self$inspect(reload)$status$state
  self$service <- function() {
    self$.parent$service$get(self$inspect(FALSE)$service_id)
  }

  stevedore_object(self, "docker_task",
                   "Work with a particular docker task")
}


docker_secret_collection <- function(parent) {
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

  stevedore_object(self, "docker_secret_collection",
                   "Manage docker swarm secrets")
}


docker_config_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$create <- docker_client_method(
    "config_create", self,
    defaults = alist(name = , data =),
    promote = c("name", "data"),
    process = list(quote(data <- validate_secret_data(data))),
    after = after_secret_create)

  self$inspect <- docker_client_method(
    "config_inspect", self)

  self$list <- docker_client_method(
    "config_list", self,
    process = list(quote(filters <- as_docker_filter(filters))),
    after = after_secret_list)

  self$remove <- docker_client_method(
    "config_delete", self)

  self$update <- docker_client_method(
    "config_update", self)

  stevedore_object(self, "docker_config_collection",
                   "Manage docker swarm configs")
}


docker_plugin_collection <- function(parent) {
  self <- new_stevedore_object(parent)

  self$list <- docker_client_method(
    "plugin_list", self)

  self$privileges <- docker_client_method(
    "plugin_privileges", self)

  self$install <- docker_client_method(
    "plugin_install", self,
    rename = c("alias" = "name"),
    fix = list(body = NULL),
    process = list(
      mcr_prepare_stream_and_close(quote(stream)),
      quote(assert_scalar_logical_or_null(grant_all)),
      quote(assert_scalar_logical(disable)),
      quote(body <- validate_plugin_privileges(object, remote, grant_all))),
    hijack = quote(streaming_json(pull_status_printer(stream))),
    allow_hijack_without_stream = TRUE,
    after = after_plugin_install)

  self$get <- docker_client_getter(docker_plugin, parent, "name")

  self$create <- docker_client_method(
    "plugin_create", self,
    rename = c(plugin_data_dir = "tar_context"),
    defaults = alist(plugin_data_dir =),
    process = list(
      quote(plugin_data_dir <- validate_tar_directory(plugin_data_dir))),
    after = after_plugin_create)

  stevedore_object(self, "docker_plugin_collection",
                   "Work with docker plugins")
}


docker_plugin <- function(name, parent) {
  self <- new_stevedore_object(parent)
  fix_name <- docker_client_add_inspect(name, "name", "plugin_inspect", self)

  self$id <- function() self$inspect(FALSE)$id
  self$is_enabled <- function(reload = TRUE) self$inspect(reload)$enabled

  self$remove <- docker_client_method(
    "plugin_remove", self,
    fix = fix_name)

  self$enable <- docker_client_method(
    "plugin_enable", self,
    fix = fix_name,
    defaults = alist(timeout = 0L),
    process = list(
      quote(timeout <- assert_scalar_integer(timeout))))

  self$disable <- docker_client_method(
    "plugin_disable", self,
    fix = fix_name)

  self$configure <- docker_client_method(
    "plugin_configure", self,
    fix = fix_name,
    process = list(
      quote(body <- validate_plugin_configure_body(body))))

  stevedore_object(self, "docker_plugin",
                   "Work with a particular docker plugin")
}


docker_types <- function(parent) {
  self <- new_stevedore_object(parent)
  types <- lapply(parent$.api_client$types, "[[", "reciever")
  list2env(types, self)
  stevedore_object(self, "docker_types",
                   "Methods for building complex docker types")
}
