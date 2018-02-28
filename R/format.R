##' @export
format.docker_stream <- function(x, ..., style = "auto",
                                 colour_stdin = "yellow",
                                 colour_stdout = "blue",
                                 colour_stderr = "red",
                                 colour_stevedore = "white",
                                 prefix_stdin = "I< ",
                                 prefix_stdout = "O> ",
                                 prefix_stderr = "E> ",
                                 prefix_stevedore = "-- ",
                                 max_lines = NULL,
                                 strip_newline = FALSE,
                                 dest = NULL,
                                 filter = NULL) {
  stream <- as.character(attr(x, "stream"))
  attributes(x) <- NULL

  if (!is.null(filter)) {
    i <- stream %in% filter
    stream <- stream[i]
    x <- x[i]
  }

  if (!is.null(max_lines) && !identical(max_lines, Inf)) {
    assert_scalar_integer(max_lines)
    n <- length(x)
    if (n > max_lines) {
      mid <- floor((max_lines - 1) / 2)
      j1 <- seq_len(mid)
      j2 <- seq(to = n, length.out = max_lines - mid - 1L)
      msg <- sprintf("[...truncated %d lines...]",
                     n - max_lines + 1L)
      x <- c(x[j1], msg, x[j2])
      stream <- c(stream[j1], "stevedore", stream[j2])
    }
  }

  if (strip_newline) {
    x <- sub("\n$", "", x)
  }

  i_i <- stream == "stdin"
  i_o <- stream == "stdout"
  i_e <- stream == "stderr"
  i_s <- stream == "stevedore"
  if (style == "auto") {
    ## I'm not 100% certain that passing this through here is
    ## necessary but this helps when output is diverted (say to a
    ## file) in not colourizing the output.
    style <- if (has_colour(dest)) "colour" else "prefix"
  }
  if (style == "plain") {
    x <- x
  } else if (style == "prefix") {
    f <- function(str, prefix) {
      paste0(prefix, gsub("\n(?=.)", paste0("\n", prefix), str, perl = TRUE))
    }
    x[i_i] <- f(x[i_i], prefix_stdin)
    x[i_o] <- f(x[i_o], prefix_stdout)
    x[i_e] <- f(x[i_e], prefix_stderr)
    x[i_s] <- f(x[i_s], prefix_stevedore)
  } else if (style == "colour") {
    x[i_i] <- crayon::style(x[i_i], colour_stdin)
    x[i_o] <- crayon::style(x[i_o], colour_stdout)
    x[i_e] <- crayon::style(x[i_e], colour_stdin)
    x[i_s] <- crayon::style(x[i_s], colour_stevedore)
  }
  x
}

##' @export
print.docker_stream <- function(x, ...) {
  cat(format(x, ...), sep = "")
  invisible(x)
}

##' @export
format.docker_container <- function(x, ..., summary = FALSE) {
  c("<docker_container>",
    sprintf("  id: %s", x$id()),
    sprintf("  name: %s", x$name()))
}

##' @export
format.docker_run_output <- function(x, ..., max_lines = 12) {
  c("<docker_run_output>",
    "  $container:",
    indent(format(x$container), 4),
    "",
    "  $logs:",
    indent(format(x$logs, ..., max_lines = max_lines, strip_newline = TRUE), 4))
}

##' @export
print.docker_run_output <- function(x, ...) {
  cat(paste0(format(x, ...), "\n"), sep = "")
  invisible(x)
}

##' @export
print.stevedore_object <- function(x, ..., indent = 2L) {
  nms <- sort(names(x))
  is_fn <- vlapply(nms, function(el) is.function(x[[el]]))

  cat(sprintf("<%s>\n", class(x)[[1]]))
  cat(sprintf("%s%s: %s\n", strrep(" " , indent), nms[!is_fn],
              vcapply(nms[!is_fn], function(el) class(x[[el]])[[1]])),
      sep = "")
  defns <- vcapply(nms[is_fn], function(el) capture_args(x[[el]], el, indent),
                   USE.NAMES = FALSE)
  cat(paste0(defns, "\n", collapse = ""))
  invisible(x)
}
