build_tar <- function(root, dockerfile) {
  ignore <- read_dockerignore(root)
  files <- build_file_list(root, ignore, dockerfile %||% "Dockerfile")
  tar_files(files, root)
}


build_file_list <- function(root, ignore, dockerfile) {
  if (is.null(ignore)) {
    return(".")
  }

  owd <- setwd(root)
  on.exit(setwd(owd))

  incl <- character()
  check <- list(list(path = ".", ignore = ignore))

  while (length(check) > 0L) {
    x <- check[[1L]]
    files <- dir(x$path, all.files = TRUE, no.. = TRUE)

    is_dir <- is_directory(file_path(x$path, files))

    incl_files <- files[!is_dir][should_include_file(files[!is_dir], x$ignore)]

    tmp <- should_include_directory(files[is_dir], x$ignore, x$path)
    incl <- c(incl, file_path(x$path, c(incl_files, tmp$keep)))
    check <- c(check[-1L], tmp$check)
  }

  if (file.exists(dockerfile) && !dockerfile %in% incl) {
    incl <- c(incl, dockerfile)
  }

  sort(incl)
}


read_dockerignore <- function(root) {
  p <- file.path(root, ".dockerignore")
  if (file.exists(p)) {
    parse_dockerignore(readLines(p))
  } else {
    NULL
  }
}


parse_dockerignore <- function(x) {
  x <- trimws(x[!grepl("^(#|\\s*$)", x)])
  if (length(x) == 0L) {
    return(NULL)
  }

  is_exception <- grepl("^!", x)
  if (any(is_exception = grepl("^!", x))) {
    x[is_exception] <- sub("^!", "", x[is_exception])
  }

  ## Normalise separators
  x <- gsub("[/\\\\]+", "/", x)
  ## Root and working directory are considered the same:
  x <- sub("^/+[^/]", "", x)

  list(patterns = x, is_exception = is_exception)
}


should_include_file <- function(path, ignore) {
  is_exception <- ignore$is_exception
  p <- glob2rx2(ignore$patterns)

  n <- length(path)

  include <- rep_len(TRUE, length(n))
  for (i in seq_along(is_exception)) {
    m <- grepl(p[[i]], path)
    if (is_exception[[i]]) {
      include <- include | m
    } else {
      include <- include & !m
    }
  }

  include
}


should_include_directory <- function(path, ignore, root) {
  if (length(path) == 0L) {
    return(NULL)
  }

  tmp <- split_path(ignore$patterns)
  p <- glob2rx2(tmp$path)
  rest <- tmp$rest
  rest[!nzchar(rest)] <- "*"

  keep <- should_include_file(path, ignore)
  check <- vector("list", length(path))

  for (i in seq_along(path)) {
    d <- path[[i]]
    m <- vlapply(p, grepl, d, USE.NAMES = FALSE)
    if (keep[i]) {
      check_i <- any(!ignore$is_exception[m])
      if (check_i) {
        keep[[i]] <- FALSE
      }
    } else {
      check_i <- any(ignore$is_exception[m])
    }
    if (check_i) {
      check[[i]] <- list(
        path = file_path(root, d),
        ignore = list(patterns = rest[m],
                      is_exception = ignore$is_exception[m]))
    }
  }

  list(keep = path[keep],
       check = check[!vlapply(check, is.null)])
}


glob2rx2 <- function(x) {
  ## Convert things into regex - different to R's glob2rx because of
  ## the double-star operator.  I'm going through a silly sentinal
  ## character here that can't possibly be in the string.  There is
  ## undoubtedly a better way of doing this!
  double_star <- "\rSTAR\r/"
  x <- sub("**/", double_star, x, fixed = TRUE)
  x <- gsub("*", "[^/]+", x, fixed = TRUE)
  x <- gsub(double_star, "([^/]+/)*", x, fixed = TRUE)
  x <- gsub("?", ".", x, fixed = TRUE)
  x <- sprintf("^%s$", x)
  x
}


split_path <- function(x) {
  re <- "^([^/]+(/|$))(.*)"
  stopifnot(all(grepl(re, x)))
  path <- sub(re, "\\1", x)
  rest <- sub(re, "\\3", x)
  i <- string_starts_with(path, "**")
  if (any(i)) {
    rest[i] <- paste0("**/", rest[i])
  }
  path <- sub("/$", "", path)
  list(path = path, rest = rest)
}
