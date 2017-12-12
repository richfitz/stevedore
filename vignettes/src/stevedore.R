## ---
## title: "Using docker with stevedore"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{Using docker with stevedore}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

## `stevedore` is an R package that interacts with docker directly
## over docker's API.  This gives a more direct access to docker than
## going via the command line.  Everything is pretty much up for grabs
## design-wise at the moment.  This vignette exists primarily so I can
## lay out a few use cases.

##+ include = FALSE
knitr::opts_chunk$set(error = FALSE)

## The main function in the package is `docker_client`; this will
## construct an object with which we can talk with the docker server.
## Currently this is hardcoded to only work with a docker server that
## is running over the default unix socket (`/var/run/docker.sock`),
## but hopefully soon this can be relaxed.
docker <- stevedore::docker_client()

## The client object looks a lot like an
## [`R6`](https://github.com/wch/R6) object, though it is implemented
## differently because the interface here is automatically generated
## in ways that don't play nicely with R6.  But if you are at all
## familiar with R6 objects it should seem quite familiar.
docker

## Each function call (e.g., `ping`) is callable by accessing with
## `$`, such as
docker$ping()

## In addition there are "collection" objects (e.g., `containers`)
## that are accessed using `$`, like a directory structure
docker$containers

## The interface is designed similarly to the command line docker
## client (and to the Python docker client), where container commands
## are within the `container` collection and image commands are within
## the `image` collection and so on (the main difference with the
## command line client is that the container commands are not at the
## top level, so it is `docker$containers$run(...)` not
## `docker$run(...)`).

## To run a container, the `docker$containers$run` command follows the
## semantics of the command line client and will
##
## * pull a image if it does not exist
## * create the container
## * run the container
## * fetch the logs
res <- docker$containers$run("hello-world")

## This returns a list with two elements:
names(res)

## The "logs" element is the logs themselves:
res$logs

## This is a `docker_stream` object and codes the output stream using
## the `stream` attribute (it can otherwise be treated as a character
## vector).

## The "container" element is an object that can be used to interact
## with a container
res$container

## For example the function `path_stat` gets some information about
## paths on the container:
res$container$path_stat("hello")

## The image can also be returned
res$container$image()

## which is another object with methods that can be invoked to find
## out about the image
