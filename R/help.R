##' Methods for working with docker containers.  This object is
##'   \code{$containers} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("containers")}
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
##' by using \code{$containers$get} to fetch an existing container by
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
##'   \code{$images} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("images")}
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
##' \code{$images$get} to fetch an existing image by name or id.
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
##'   \code{$networks} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("networks")}
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
##' \code{$networks$get} to fetch an existing network by name or id.
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
##'   \code{$volumes} within a \code{\link{docker_client}} object.
##'
##' \Sexpr[results=rd,stage=render]{stevedore:::generate_help("volumes")}
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
##' \code{$volumes$get} to fetch an existing volume by name or id.
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


generate_help <- function(sub = NULL, api_version = NULL) {
  oo <- options(stevedore.silent = TRUE)
  on.exit(options(oo))
  tryCatch(generate_help_string(sub, api_version),
           error = function(e) "(automatic help generation has failed)")
}


generate_help_string <- function(sub = NULL, api_version = NULL) {
  ## We should store the last used version in a cache I think?
  x <- docker_client(api_version, http_client_type = "null")
  api_version <- x$api_version()
  if (!is.null(sub)) {
    if (sub %in% names(x)) {
      x <- x[[sub]]
    } else {
      api_client <- docker_api_client(api_version = x$api_version(),
                                      type = "null")
      f <- switch(sub,
                  docker_container = docker_client_container,
                  docker_image = docker_client_image,
                  docker_network = docker_client_network,
                  docker_volume = docker_client_volume,
                  docker_exec = docker_client_exec,
                  stop("impossible!"))
      x <- f(dummy_id(), api_client)
    }
  }

  nms <- sort(names(x))
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
    "of the docker API - other versions are available.  This documentation",
    "is automatically generated from docker's API schema, and so",
    "inaccuracies may exist between it and stevedore's interface",
    "(especially references to JSON objects).  Please report any",
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
    ## blank line or the end.  Otherwise assume only one group of
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
  call <- capture_args(x, name, 0L)
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
  summary <- h$summary
  if (!is.null(h$description)) {
    summary <- sprintf("%s.  %s", summary, h$description)
  }
  if (!is.null(h$cli)) {
    cli <- paste(sprintf("`docker %s`", h$cli), collapse = " or ")
    summary <- sprintf("%s  Similar to the cli command %s.",
                       summary, cli)
  }
  summary
}
