daemon_df <- function(client) {
  url <- client$url('/system/df')
  client$GET(url, as = "json")$data
}
daemon_info <- function(client) {
  url <- client$url('/info')
  client$GET(url, as = "json")$data
}
daemon_ping <- function(client) {
  url <- client$url('/_ping')
  client$GET(url, as = "text")$data == "OK"
}
daemon_version <- function(client, versioned_api = TRUE) {
  url <- client$url('/version', versioned_api = versioned_api)
  client$GET(url, as = "json")$data
}

## These can wait:
daemon_events <- function(client) {
  .NotYetImplemented()
}
daemon_login <- function(client) {
  .NotYetImplemented()
}
