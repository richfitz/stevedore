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
##'   network objects.
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


generate_help <- function(sub = NULL, api_version = NULL) {
  tryCatch(generate_help_string(sub, api_version),
           error = function(e) "(automatic help generation has failed)")
}


generate_help_string <- function(sub = NULL, api_version = NULL) {
  ## We should store the last used version in a cache I think?
  x <- docker_client(api_version)
  if (!is.null(sub)) {
    if (sub %in% names(x)) {
      x <- x[[sub]]
    } else {
      x <- switch(sub,
                  docker_container = x$containers$get(HELP),
                  stop("impossible!"))
    }
  }

  nms <- sort(names(x))
  is_fn <- vlapply(nms, function(el) is.function(x[[el]]))
  fns <- vcapply(nms[is_fn], generate_docker_client_method_rd, x,
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

  c(mgmt,
    "\\subsection{Methods}{",
    "\\describe{",
    fns,
    "}",
    "}")
}


markdown_to_rd <- function(str) {
  if (grepl("\n+(\\s*-\\s+)", str)) {
    ## Simplest thing that might work - keep going until the next
    ## blank line or the end.
    tmp <- strsplit(str, "\n", fixed = TRUE)[[1]]

    blank <- grep("^\\s*$", tmp)
    item_start <- grep("^\\s*-\\s+", tmp)
    ## simplest case:
    stopifnot(length(blank) == 1L, all(item_start > blank))
    ## which implies:
    item_end <- c(item_start[-1L] - 1L, length(tmp))
    tmp[item_start] <- sub("^\\s*-\\s+", "\\\\item ", tmp[item_start])
    tmp[blank] <- "\\itemize{"
    tmp <- c(tmp, "}")
    str <- paste0(tmp, "\n", collapse = "")
  }

  gsub("`([^`]+)`", "\\\\code{\\1}", str)
}


## Fold in with format.docker_client_method?
generate_docker_client_method_rd <- function(name, obj) {
  x <- obj[[name]]
  call <- capture_args(x, name, 0L)
  h <- attr(x, "help")
  if (is.null(h)) {
    summary <- "(documentation not yet available)"
  } else if (is.null(h$description)) {
    summary <- h$summary
  } else {
    summary <- sprintf("%s.  %s", h$summary, h$description)
  }
  summary <- markdown_to_rd(summary)

  if (is.null(h$args)) {
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
          sprintf("\\code{%s}", call),
          args,
          "}"), sep = "\n", collapse = "\n")
}
