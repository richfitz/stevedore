daemon_df <- function(api_client) {
  url <- api_client$url('/system/df')
  api_client$GET(url, as = "json")
}
daemon_info <- function(api_client) {
  url <- api_client$url('/info')
  api_client$GET(url, as = "json")
}
daemon_ping <- function(api_client) {
  url <- api_client$url('/_ping')
  api_client$GET(url, as = "text") == "OK"
}
daemon_version <- function(api_client) {
  url <- api_client$url('/version')
  api_client$GET(url, as = "json")
}

## These can wait:
daemon_events <- function(api_client) {
  .NotYetImplemented()
}
daemon_login <- function(api_client) {
  .NotYetImplemented()
}
