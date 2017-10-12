image_history <- function(client, image) {
  ## TODO: it's not really clear but we can do _much_better with the
  ## output here, especially as the schema is pretty clear about what
  ## we expect.  This looks like the classic use case of a data.frame.
  ## I should come up with a schema -> df converter.
  ##
  ## Similarly, error codes are pretty clear; we expect 404 to be
  ## missing image
  url <- client$url("/images/%s/history", image)
  client$GET(url, as = "json")$data
}

image_list <- function(client, name = NULL) {
  params <- list()
  if (!is.null(name)) {
    params$filter <- name
  }
  url <- client$url("/images/json", params = params)
  res <- client$GET(url, "json")$data
  res
}

## image_list <- function(client, name = NULL, quiet = FALSE,
##                        all = FALSE, filters = NULL) {
##   params <- list(filter = name,
##                  only_ids = if (quiet) 1L, else 0L,
##                  all = if (all) 1L else 0L)
##   if (!is.null(filters)) {
##     browser()
##   }
##   url <- client$url("/images/json", params = params)

##   cl$GET("http://localhost/images/json")
##   res <- client$GET(url, "json")
##   if (quiet) {
##     vcapply(res, "[[", "Id")
##   } else {
##     res
##   }
## }
