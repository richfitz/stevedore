devtools::load_all()

docker_spec_fetch <- function(dest) {
  vcapply(swagger_spec_versions(), docker_spec_fetch1, dest)
}


docker_spec_fetch1 <- function(version, dest) {
  url <- sprintf("https://docs.docker.com/engine/api/v%s/swagger.yaml", version)
  dest_file <- file.path(dest, sprintf("v%s.yaml", version))
  download_file(url, dest_file)
  bzip_file(dest_file)
}


bzip_file <- function(path) {
  dest <- paste0(path, ".bz2")
  dat <- read_binary(path)
  con <- bzfile(dest, "wb", compression = 9L)
  on.exit(close(con))
  writeBin(dat, con)
  dest
}


docker_spec_fetch("inst/spec")
