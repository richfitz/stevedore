docker_client_method <- function(name, object,
                                 fix = NULL, rename = NULL,
                                 drop = NULL, defaults = NULL,
                                 expand = NULL,
                                 promote = NULL, process = NULL,
                                 after = NULL,
                                 hijack = NULL,
                                 allow_hijack_without_stream = FALSE) {
  api_client <- object$.parent$.api_client

  stopifnot(c("endpoints", "http_client") %in% names(api_client))
  endpoint <- api_client$endpoints[[name]]
  if (isTRUE(endpoint$unsupported)) {
    return(docker_client_method_unsupported(endpoint, name))
  }

  ## TODO: does this mean that contents of fenv collide with the names
  ## of parameters?  If so this should be the *parent* env possibly?
  ## Or just dot name these.
  fenv <- new.env(parent = api_client, hash = FALSE)
  fenv$endpoint <- endpoint
  fenv$api_client <- api_client # neeed?
  fenv$object <- object
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

  if (!is.null(endpoint$extra)) {
    args_use <- c(args_use, endpoint$extra)
  }

  if (!is.null(expand)) {
    types <- object$.parent$types
    stopifnot(all(expand %in% names(types)))
    expand <- as.list(expand)

    for (i in seq_along(expand)) {
      arg <- names(expand)[[i]]
      type <- expand[[i]]
      stopifnot(arg %in% names(args_use))

      type_fn <- types[[type]]
      type_args <- formals(type_fn)
      type_help <- attr(type_fn, "help")$args

      ## The downside of this approach is that if additional colliding
      ## arguments are given in future versions, this will require
      ## backward-incompatible name changes.
      fix_name <- names(type_args) %in% names(args_use)
      if (any(fix_name)) {
        stopifnot(identical(names(type_help), names(type_args)))
        names(type_args)[fix_name] <-
          sprintf("%s_%s", type, names(type_args)[fix_name])
        names(type_help)[fix_name] <-
          sprintf("%s_%s", type, names(type_help)[fix_name])
      }

      j <- match(arg, names(args_use))
      args_use <- c(append(args_use[-j], type_args, j - 1L), args_use[j])
      p <- process_expanded_arg(as.name(arg), type, names(type_args))

      expand[[i]] <- list(process = p, help = type_help)
    }

    ## Now, run the process args _backwards_ to deal with nested types
    process <- c(rev(unname(lapply(expand, "[[", "process"))), process)
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
  if (is.null(endpoint$extra)) {
    add_extra <- NULL
  } else {
    add_extra <- bquote(
      params[.(as.call(c(quote(c), names(endpoint$extra))))] <-
        .(as.call(c(quote(list), lapply(names(endpoint$extra), as.name)))))
  }

  if (!is.null(after)) {
    fenv$after <- after
    finish <- c(call("<-", quote(response), run_endpoint),
                add_extra,
                quote(after(response, params, object)))
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
  if (!is.null(expand)) {
    for (i in seq_along(expand)) {
      arg <- names(expand)[[i]]
      help$args[[arg]] <- sprintf(
        "%s.  If this is given then %s must all be NULL.",
        help$args[[arg]],
        join_text_list(squote(names(expand[[i]]$help))))
      help$args <- c(help$args, expand[[i]]$help)
    }
  }
  if (length(help$args) > 0L || length(args_use) > 0L) {
    stopifnot(all(names(args_use) %in% names(help$args)))
    help$args <- help$args[names(args_use)]
  }
  help$name <- name

  attr(ret, "help") <- help
  attr(ret, "name") <- name

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
  str <- format.docker_client_method(x, "text",
                                     indent = indent, exdent = exdent,
                                     args = args, ...)
  cat(paste0(str, "\n", collapse = ""))
  invisible(x)
}


##' @export
format.docker_client_method <- function(x, type = "text", ...) {
  type <- match_value(type, c("text", "rd"))
  if (type == "text") {
    format_docker_client_method_text(x, ...)
  } else {
    format_docker_client_method_rd(x, ...)
  }
}


process_expanded_arg <- function(dest, typename, args) {
  collect <- as.call(c(list(bquote(api_client$types[[.(typename)]]$reciever)),
                       lapply(args, as.name)))
  assert_null <- as.call(c(list(quote(assert_arg_is_null), as.character(dest)),
                           set_names(lapply(args, as.name), args)))
  substitute(
    if (is.null(dest)) {
      dest <- collect
    } else {
      assert_null
      dest <- api_client$types[[typename]]$handler(dest)
    },
    list(dest = as.name(dest), typename = typename, collect = collect,
         assert_null = assert_null))
}
