##' Methods for working with docker containers.  This object is
##'   \code{$container} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("container")}
##'
##' @name docker_container_collection
##'
##' @title Management commands for working with docker containers
##'
##' @seealso \code{\link{docker_container}} for information on
##'   container objects.
NULL


##' Methods for working with a particular docker container.  Container
##' objects are returned by creating or running a docker container, or
##' by using \code{$container$get} to fetch an existing container by
##' name or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_container")}
##'
##' @name docker_container
##'
##' @title Management commands for working with a particular docker container
##'
##' @seealso \code{\link{docker_container_collection}} for other
##'   container management methods.
NULL


##' Methods for working with docker images.  This object is
##'   \code{$image} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("image")}
##'
##' @name docker_image_collection
##'
##' @title Management commands for working with docker images
##'
##' @seealso \code{\link{docker_image}} for information on
##'   image objects.
NULL


##' Methods for working with a particular docker image.  Image objects
##' are returned by building or pulling a docker image, or by using
##' \code{$image$get} to fetch an existing image by name or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_image")}
##'
##' @name docker_image
##'
##' @title Management commands for working with a particular docker image
##'
##' @seealso \code{\link{docker_image_collection}} for other
##'   image management methods.
NULL


##' Methods for working with docker networks.  This object is
##'   \code{$network} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("network")}
##'
##' @name docker_network_collection
##'
##' @title Management commands for working with docker networks
##'
##' @seealso \code{\link{docker_network}} for information on
##'   network objects.
NULL


##' Methods for working with a particular docker network.  Network
##' objects are returned by creating a docker network, or by using
##' \code{$network$get} to fetch an existing network by name or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_network")}
##'
##' @name docker_network
##'
##' @title Management commands for working with a particular docker network
##'
##' @seealso \code{\link{docker_network_collection}} for other
##'   network management methods.
NULL


##' Methods for working with docker volumes.  This object is
##'   \code{$volume} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("volume")}
##'
##' @name docker_volume_collection
##'
##' @title Management commands for working with docker volumes
##'
##' @seealso \code{\link{docker_volume}} for information on
##'   volume objects.
NULL


##' Methods for working with a particular docker volume.  Volume
##' objects are returned by creating a docker volume, or by using
##' \code{$volume$get} to fetch an existing volume by name or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_volume")}
##'
##' @name docker_volume
##'
##' @title Management commands for working with a particular docker volume
##'
##' @seealso \code{\link{docker_volume_collection}} for other
##'   volume management methods.
NULL


##' Methods for working with docker "exec" instances, which are
##' returned by running \code{exec} on a running container
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_exec")}
##'
##' @name docker_exec
##'
##' @title Commands for working with a docker exec instance
##'
##' @seealso \code{\link{docker_container}}
NULL


##' Methods for managing the docker swarm.  This object is
##'   \code{$swarm} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("swarm")}
##'
##' @name docker_swarm_collection
##'
##' @title Management commands for working with docker swarm
NULL


##' Methods for managing docker swarm nodes.  This object is
##'   \code{$node} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("node")}
##'
##' @name docker_node_collection
##'
##' @title Management commands for working with swarm nodes
##'
##' @seealso \code{\link{docker_swarm_collection}} for management
##'   commands for the swarm itself.
NULL


##' Methods for working with a particular docker node.  Node objects
##' are by using \code{$node$get} to fetch an existing node by name or
##' id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_node")}
##'
##' @name docker_node
##'
##' @title Management commands for working with a particular docker node
##'
##' @seealso \code{\link{docker_node_collection}} for other
##'   node management methods.
NULL


##' Methods for working with docker services.  This object is
##'   \code{$service} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("service")}
##'
##' @name docker_service_collection
##'
##' @title Management commands for working with docker services
##'
##' @seealso \code{\link{docker_service}} for information on
##'   service objects.
NULL


##' Methods for working with a particular docker service.  Service
##' objects are returned by creating a docker service, or by using
##' \code{$service$get} to fetch an existing service by name or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_service")}
##'
##' @name docker_service
##'
##' @title Management commands for working with a particular docker service
##'
##' @seealso \code{\link{docker_service_collection}} for other
##'   service management methods.
NULL


##' Methods for working with docker tasks.  This object is
##'   \code{$task} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("task")}
##'
##' @name docker_task_collection
##'
##' @title Management commands for working with docker tasks
##'
##' @seealso \code{\link{docker_task}} for information on
##'   task objects.
NULL


##' Methods for working with a particular docker task.  Task objects
##' are returned by using \code{$task$get} to fetch an existing task
##' by name or id, or \code{$tasks} from a
##' \code{\link{docker_service}} object representing a docker service.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_task")}
##'
##' @name docker_task
##'
##' @title Management commands for working with a particular docker task
##'
##' @seealso \code{\link{docker_task_collection}} for other
##'   task management methods.
NULL


##' Methods for managing docker swarm secrets.  This object is
##'   \code{$secret} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("secret")}
##'
##' @name docker_secret_collection
##'
##' @title Management commands for working with swarm secrets
##'
##' @seealso \code{\link{docker_swarm_collection}} for management
##'   commands for the swarm itself, and
##'   \code{\link{docker_config_collection}} for a similar inteface
##'   for configuring non-sensitive configurations.
NULL


##' Methods for managing docker swarm configs.  This object is
##'   \code{$config} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("config")}
##'
##' @name docker_config_collection
##'
##' @title Management commands for working with swarm configs
##'
##' @seealso \code{\link{docker_swarm_collection}} for management
##'   commands for the swarm itself, and
##'   \code{\link{docker_secret_collection}} for a similar inteface
##'   for configuring sensitive configurations.
NULL


##' Methods for working with docker plugins.  This object is
##'   \code{$plugin} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("plugin")}
##'
##' @name docker_plugin_collection
##'
##' @title Management commands for working with docker plugins
##'
##' @seealso \code{\link{docker_plugin}} for information on
##'   plugin objects.
NULL


##' Methods for working with a particular docker plugin.  Plugin
##' objects are returned by installing or building a docker plugin, or
##' by using \code{$plugin$get} to fetch an existing plugin by name
##' or id.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_plugin")}
##'
##' @name docker_plugin
##'
##' @title Management commands for working with a particular docker plugin
##'
##' @seealso \code{\link{docker_plugin_collection}} for other
##'   plugin management methods.
NULL


##' Methods for building complex docker types.  This is most objects
##' more complicated than R's atomic types.  Most functions will
##' indicate if they require one of these objects in their help.  None
##' of these functions do anything interesting in their own regard -
##' they just validate inputs.
##'
##' The functions here will all depend on the API versions - some of
##' the most fluid parts of the docker API are the different options
##' that are supported via things like \code{host_config}.
##'
##' These functions are needed because \code{stevedore} aims to be a
##' fairly direct wrapping aroud the docker API.  For most of the
##' single host methods the types here are not really used (with the
##' notable exception of \code{host_config} which is used by
##' \code{$container$create} and \code{$container$update}).  But for
##' the swarm endpoints the function definitions would be impossibly
##' complex if we did not reflect the types.  So rather than one
##' function call with a hundred arguments, we can build up the
##' required types.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("types")}
##'
##' @title Constructors for complex types
##'
##' @name docker_types
NULL


generate_help <- function(sub = NULL, api_version = NULL) {
  tryCatch(generate_help_string(sub, api_version),
           error = function(e) "(automatic help generation has failed)")
}


generate_help_string <- function(sub = NULL, api_version = NULL) {
  ## We should store the last used version in a cache I think?
  x <- docker_client(api_version, http_client_type = "null", quiet = TRUE)
  api_version <- x$api_version()
  if (!is.null(sub)) {
    if (sub %in% names(x)) {
      x <- x[[sub]]
    } else {
      f <- switch(sub,
                  docker_container = docker_container,
                  docker_image = docker_image,
                  docker_network = docker_network,
                  docker_volume = docker_volume,
                  docker_exec = docker_exec,
                  docker_node = docker_node,
                  docker_service = docker_service,
                  docker_task = docker_task,
                  docker_plugin = docker_plugin,
                  stop("impossible!"))
      x <- f(dummy_id(), x)
    }
  }

  nms <- ls(x)
  is_fn <- vlapply(nms, function(el) is.function(x[[el]]))
  fns <- vcapply(nms[is_fn], function(nm)
    format_docker_client_method_rd(x[[nm]]),
    USE.NAMES = FALSE)

  if (all(is_fn)) {
    mgmt <- NULL
  } else {
    stopifnot(is.null(sub))
    extra <- nms[!is_fn]
    link <- sprintf("docker_%s_collection", sub("s$", "", extra))
    fmt <- "\\item{\\code{%s}}{Manage %s - see \\code{\\link{%s}}}"
    items <- sprintf(fmt, extra, extra, link)
    mgmt <- c("\\subsection{Management commands}{",
              "\\describe{",
              items,
              "}",
              "}",
              "")
  }

  preamble <- c(
    "Below is reference documentation for all methods for version",
    squote(api_version),
    "of the docker API - other versions are available. This documentation",
    "is automatically generated from docker's API schema, and so",
    "inaccuracies may exist between it and stevedore's interface",
    "(especially references to JSON objects). Please report any",
    "documentation that might be improved at",
    "https://github.com/richfitz/stevedore/issues")

  c(preamble,
    "",
    mgmt,
    "\\subsection{Methods}{",
    "\\describe{",
    fns,
    "}",
    "}")
}


markdown_to_rd <- function(str) {
  if (grepl("\n+(\\s*-\\s+)", str)) {
    ## Simplest thing that might work - keep going until the next
    ## blank line or the end. Otherwise assume only one group of
    ## items
    tmp <- strsplit(str, "\n", fixed = TRUE)[[1]]

    blank <- grep("^\\s*$", tmp)
    item_start <- grep("^\\s*-\\s+", tmp)
    end <- length(tmp) + 1L

    if (length(blank) > 1L) {
      stopifnot(!any(blank > item_start[[1]] & blank < max(item_start)))
      blank2 <- blank[blank > max(item_start)]
      if (length(blank2) > 0L) {
        end <- blank2[[1]]
      }
      blank <- max(blank[blank < item_start[[1]]])
    }

    tmp[item_start] <- sub("^\\s*-\\s+", "\\\\item ", tmp[item_start])
    tmp[blank] <- "\\itemize{"
    tmp[end] <- "}"
    str <- paste0(tmp, "\n", collapse = "")
  }

  str <- gsub("```([^`]+)```", "\\\\preformatted{\\1}", str)
  str <- gsub("`([^`]+)`", "\\\\code{\\1}", str)
  str
}


markdown_to_text <- function(str, colour = crayon::has_color()) {
  if (colour) {
    open <- "\033[1m"
    close <- "\033[22m"
    repl <- sprintf("%s\\1%s", open, close)
    str <- gsub("```([^`]+)```", repl, paste(str, collapse = "\n"))
    str <- gsub("`([^`]+)`", repl, str)
  }
  str
}


format_docker_client_method_text <- function(x, indent = 2, exdent = 8,
                                             args = TRUE, ...) {
  call <- capture_args(x, "function", 0L)
  divider <- strrep("-", max(nchar(strsplit(call, "\n", fixed = TRUE)[[1]])))
  h <- attr(x, "help")

  summary <- help_summary(h)
  summary <- strwrap(summary, indent = 0, exdent = indent)

  if (!args || is.null(h$args)) {
    args <- NULL
  } else {
    indent <- 2
    exdent <- 8
    f <- function(nm, txt) {
      txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      txt1 <- strwrap(sprintf("%s: %s", crayon::bold(nm), txt[[1]]),
                      indent = indent, exdent = exdent)
      txt2 <- strwrap(txt[-1], indent = exdent, exdent = exdent)
      c(txt1, txt2)
    }
    args <- mapply(f, names(h$args), unname(h$args), SIMPLIFY = FALSE)
    args <- c(divider, unlist(unname(args)))
  }

  c(call, divider, markdown_to_text(summary), markdown_to_text(args))
}


format_docker_client_method_rd <- function(x, ...) {
  h <- attr(x, "help")
  ## stopifnot(!is.null(h$name))
  name <- h$name
  call <- capture_args(x, name, 2L, getOption("width") - 13L)
  summary <- help_summary(h)
  summary <- markdown_to_rd(summary)

  if (length(h$args) == 0L) {
    args <- NULL
  } else {
    f <- function(nm, txt) {
      sprintf("\\item \\code{%s}: %s",
              nm, markdown_to_rd(txt))
    }
    txt <- mapply(f, names(h$args), unname(h$args),
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    args <- sprintf("\n\\emph{Arguments:}\n\\itemize{\n%s\n}",
                    paste0(vcapply(txt, identity), collapse = "\n"))
  }

  paste(c(sprintf("\\item{\\code{%s}}{", name),
          summary,
          "",
          "\\emph{Usage:}",
          sprintf("\\preformatted{%s}", call),
          args,
          "}"), sep = "\n", collapse = "\n")
}


help_summary <- function(h) {
  summary <- trimws(h$summary)
  if (!is.null(h$description)) {
    summary <- sprintf("%s. %s", summary, trimws(h$description))
  }
  if (!is.null(h$cli)) {
    cli <- paste(sprintf("`docker %s`", h$cli), collapse = " or ")
    if (!grepl("\\.$", summary)) {
      summary <- paste0(summary, ".")
    }
    summary <- sprintf("%s Similar to the cli command %s.",
                       summary, cli)
  }
  summary
}
