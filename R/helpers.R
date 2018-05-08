user <- function(name = NULL, group = FALSE, numeric = TRUE) {
  if (numeric) {
    is_windows <- is_windows()
    id <- numeric_id(name, FALSE, is_windows)
    if (group) {
      id <- sprintf("%s:%s", id, numeric_id(name, TRUE, is_windows))
    }
    id
  } else {
    user_name(name)
  }
}


user_name <- function(name) {
  if (is.null(name)) {
    e <- Sys.getenv()
    for (v in c("LOGNAME", "USER", "LNAME", "USERNAME")) {
      if (!is.na(e[v]) && nzchar(e[v])) {
        return(e[[v]])
      }
    }
    stop("could not determine username")
  } else {
    assert_scalar_character(name)
    name
  }
}


numeric_id <- function(name, group, is_windows) {
  if (is_windows) {
    stop("Cannot return numeric id on windows")
  }
  res <- system3("id", c(if (group) "-g" else "-u", name))
  if (!res$success) {
    stop("'id' failed with message: ", squote(res$output))
  }
  res$output
}
