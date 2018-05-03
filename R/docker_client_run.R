## This is complicated enough that it gets its own file.  The 'run'
## function is a bit of a beast.  This duplicates much of the `docker
## run` functionality.
make_docker_run <- function(client) {
  can_stream <- client$.api_client$http_client$can_stream

  ## TODO: this should pick up all the args from create rather than
  ## using dots.
  function(image, cmd = NULL, ..., detach = FALSE, rm = FALSE,
           stream = stdout(), host_config = NULL) {
    stream_data <- validate_stream(stream)
    if (stream_data$close) {
      on.exit(close(stream_data$stream), add = TRUE)
    }

    if (rm && detach) {
      ## This is supported in API 1.25 and up - which agrees with our
      ## API support.
      ##
      ## NOTE: Must use PascalCase here because this is directly
      ## passed though - at the moment!  If I get this fixed up that
      ## might need to change (the manual unboxing would also not be
      ## needed).
      host_config$AutoRemove <- jsonlite::unbox(TRUE)
    }
    image <- docker_get_image(image, client)
    container <- client$container$create(image, cmd, ...,
                                         host_config = host_config)
    if (rm && !detach) {
      ## TODO: this should be configurable - do we want to background
      ## on interrupt?
      on.exit(container$remove(), add = TRUE)
    }
    container$start()
    if (detach) {
      return(container)
    }

    if (can_stream) {
      out <- container$logs(stream = stream_data$stream, follow = TRUE)
      exit_status <- container$wait()$exit_code
    } else {
      ## TODO: warn here that this will block?
      exit_status <- container$wait()$exit_code
      out <- container$logs()
      ## NOTE: this duplicates some of the logic in print.docker_stream
      if (!is.null(stream_data$stream)) {
        cat(format(out, stream = stream_data$stream),
            file = stream_data$stream, sep = "")
      }
    }

    if (rm) {
      container$inspect(TRUE)
    }
    if (exit_status != 0L) {
      stop(container_error(container, exit_status, cmd, image, out))
    }
    ret <- list(container = container, logs = out)
    class(ret) <- "docker_run_output"
    ret
  }
}


## This supports the same semantics as docker run, where images are
## pulled if they are not found locally.
docker_get_image <- function(image, client, name = deparse(substitute(image))) {
  if (inherits(image, "docker_image")) {
    image
  } else {
    image <- image_name_with_tag(image, name)
    tryCatch(
      client$image$get(image),
      docker_error = function(e) {
        if (is_docker_error_not_found(e)) {
          message(sprintf("Unable to find image '%s' locally", image))
          client$image$pull(image)
        } else {
          stop(e)
        }
      })
  }
}
