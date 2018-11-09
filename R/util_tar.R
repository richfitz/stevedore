## R's tarring of selected files from within a directory is broken!
## r-lib/tar, after PR #1, has a fix that will allow the internal tar
## method to work, but that package is unlikely to get a lot of work
## done as Gabor says that it is currently abandonned.  Using system
## tar seems to work and that should be ok on Linux and macOS (so the
## two platforms that docker will be most used on, and where we have
## decent http support) so forcing system tar seems ok for now.

## If the r-lib/tar package goes up on CRAN it would make a reasonable
## dependency and might be slightly nicer than shelling out.

## The non-internal method of tar is broken in R 3.4.1 but I think
## fixed by ~3.4.3

## Three tar functions here:
##
## * tar_directory - tar up a directry 'root', including all files.
##   root/a/b becomes a/b within the archive
## * tar_files - like tar_directory, but only some files
## * tar_file - tar a single file at the root of its own archive

## All tar commands return a raw vector (not a file).  This means
## everything must fit in memory (potentially a couple of times as the
## docker daemon is typically on the same host) so it might be better
## to return a path.  The advantage of returning a binary stream is
## that's easy to send without worrying about streaming to httppipe
## and we get cleanup for free.

## TODO: if we're going to use the system tar it might be better to
## offer to stream from disk?

## TODO: what about compression?  Probably quite hard to do portably,
## but 'z' is pretty widely supported.

tar_directory <- function(root) {
  assert_directory(root)

  owd <- setwd(root)
  on.exit(setwd(owd))
  tmp <- tempfile("stevedore_tar_", fileext = ".tar")
  on.exit(unlink(tmp), add = TRUE)

  tar_safe(tmp, ".")
  readBin(tmp, raw(), file.size(tmp))
}


tar_files <- function(files, root, external_list = length(files) > 20) {
  assert_directory(root)

  owd <- setwd(root)
  on.exit(setwd(owd))
  tmp <- tempfile("stevedore_tar_", fileext = ".tar")
  on.exit(unlink(tmp), add = TRUE)

  tar_safe(tmp, files, complex = TRUE, external_list = external_list)
  readBin(tmp, raw(), file.size(tmp))
}


tar_file <- function(file) {
  assert_scalar_character(file)
  assert_file_exists(file)
  tar_files(basename(file), dirname(file))
}


tar_safe <- function(tarfile, files, ..., complex = FALSE,
                     external_list = FALSE) {
  assert_file_exists(files)
  if (external_list) {
    list <- tempfile("stevedore_tar_")
    on.exit(unlink(list))
    with_connection(list, open = "wb", function(con) {
      writeLines(files, con, sep = "\n")
    })
    files <- c("-T", list)
  }
  if (complex) {
    tar_system(tarfile, files, ...)
  } else {
    utils::tar(tarfile, files, ...)
  }
  tarfile
}


tar_system <- function(tarfile, files) {
  ## On mac may need to set:
  ##
  ## COPYFILE_DISABLE=1
  ##
  ## (see ?tar, and check Sys.info()[["sysname"]] == "Darwin")
  flags <- c("-c", "-f", tarfile)
  res <- system3(sys_which("tar"), c(flags, files))
  if (!res$success) {
    stop(sprintf("tar failed with status %d:\n%s",
                 res$code, paste(res$output, collapse = "\n")),
         call. = FALSE)
  }
}
