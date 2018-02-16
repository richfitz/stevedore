docker_client_method <- function(name, client, fix = NULL, rename = NULL,
                                 drop = NULL, defaults = NULL, extra = NULL,
                                 promote = NULL, process = NULL, after = NULL,
                                 hijack = NULL,
                                 allow_hijack_without_stream = FALSE) {
  stopifnot(c("endpoints", "http_client") %in% names(client))
  endpoint <- client$endpoints[[name]]
  if (isTRUE(endpoint$unsupported)) {
    return(docker_client_method_unsupported(endpoint, name))
  }

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
    assert_null(names(process))
    ## I'm not 100% sure about how needed this is, but this slightly
    ## odd formulation will eliminate the `{` blocks that otherwise
    ## turn up here.  Total cost is ~26us for the worst case
    ## (container_create).
    i <- vlapply(process, function(x) identical(x[[1L]], quote(`{`)))
    if (any(i)) {
      process[i] <- lapply(process[i], function(x) as.list(x[-1L]))
      process <- unlist(process, FALSE, FALSE)
    }
  }

  if (!is.null(hijack)) {
    assert_is(hijack, c("call", "if"))
  }

  get_params <- as.call(c(list(quote(endpoint$argument_handler)),
                          lapply(names(args), as.name)))
  run_endpoint <- substitute(
    run_endpoint(http_client, endpoint, params, hijack = hijack,
                 allow_hijack_without_stream = allow_hijack_without_stream),
    list(hijack = hijack,
         allow_hijack_without_stream = allow_hijack_without_stream))
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

  ret <- as.function(c(args_use, as.call(body)), fenv)
  class(ret) <- "docker_client_method"

  help <- endpoint$help
  if (!is.null(rename)) {
    i <- match(rename, names(help$args))
    names(help$args)[i] <- names(rename)
  }
  if (!is.null(extra)) {
    ## TODO: pull in some decent help here from somewhere.  I am
    ## punting on this for a bit as I might move the definitions into
    ## yaml which would provide a much nicer place to put the extra
    ## args than in code.
    help$args[names(extra)] <- names(extra)
  }
  if (length(help$args) > 0L || length(args_use) > 0L) {
    stopifnot(all(names(args_use) %in% names(help$args)))
    help$args <- help$args[names(args_use)]
  }

  attr(ret, "help") <- help

  ret
}


docker_client_method_unsupported <- function(endpoint, name) {
  force(endpoint)
  msg <- sprintf(
    "'%s' (%s %s) requires docker API version at least %s (version %s used)",
    endpoint$name, endpoint$method, endpoint$path,
    endpoint$version_required, endpoint$version_used)

  ret <- function(...) {
    stop(msg)
  }
  help <- docker_api_client_help("unsupported", name)
  stopifnot(!is.null(help$summary))
  help$description <- paste0("Unimplemented, as ", msg)
  help$args <- list("..." = "Ignored in this version")
  attr(ret, "help") <- help
  class(ret) <- "docker_client_method"
  ret
}

##' @export
print.docker_client_method <- function(x, indent = 2, exdent = 8, args = TRUE,
                                       ...) {
  call <- capture_args(x, "function", 0L)
  divider <- strrep("-", max(nchar(strsplit(call, "\n", fixed = TRUE)[[1]])))
  h <- attr(x, "help")
  if (is.null(h$description)) {
    summary <- h$summary
  } else {
    summary <- sprintf("%s: %s", h$summary, h$description)
  }
  summary <- strwrap(summary, indent = 0, exdent = indent)
  if (!args || is.null(h$args)) {
    args <- NULL
  } else {
    indent <- 2
    exdent <- 8
    f <- function(nm, txt) {
      txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      txt1 <- strwrap(sprintf("%s: %s", nm, txt[[1]]),
                         indent = indent, exdent = exdent)
      txt2 <- strwrap(txt[-1], indent = exdent, exdent = exdent)
      c(txt1, txt2)
    }
    args <- mapply(f, names(h$args), unname(h$args), SIMPLIFY = FALSE)
    args <- c(divider, unlist(unname(args)))
  }
  str <- c(call, divider, summary, args)
  cat(paste0(str, "\n", collapse = ""))
  invisible(x)
}
