## ---
## title: "Cookbook"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{Cookbook}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ include = FALSE
knitr::opts_chunk$set(error = FALSE)
lang_output <- function(x, lang) {
  cat(c(sprintf("```%s", lang), x, "```"), sep="\n")
}
c_output <- function(x) lang_output(x, "cc")
r_output <- function(x) lang_output(x, "r")
plain_output <- function(x) lang_output(x, "plain")

## This vignette contains short snippets of `stevedore` calls to
## perform simple tasks, alongside a docker command line equivalent.

## Conventions:

## * We assume that a `stevedore` docker client object has been
##   created with `docker <- stevedore::docker_client()` (to avoid
##   repetition in examples)
## * All docker command line examples start with `$` to represent the
##   unix/bash prompt

docker <- stevedore::docker_client()

## ## Start a container with ports published to the host

##+ include=FALSE
docker$image$pull("gplates/gws")

## With docker cli:

## ```
## $ docker run --rm -p 8888:80 gplates/gws
## ```

## the `-p 8888:80` publishes port `80` in the container to port
## `8888` on the host.

## In `stevedore`, this can be done with:
gws <- docker$container$run("gplates/gws", ports = "8888:80",
                            detach = TRUE, rm = TRUE)
gws$ports()

##+ include = FALSE
gws$kill()

## To use a random port, omit the host portion (here `8888:`) as

## ```
## $ docker run --rm -p 80 gplates/gws
## ```

## or, with `stevedore`
gws <- docker$container$run("gplates/gws", ports = 80,
                            detach = TRUE, rm = TRUE)

## To determine the port you can use the `ports()` method
gws$ports()

##+ include = FALSE
gws$kill()

## If the container declares its exposed ports using an `EXPOSE
## <port>` directive in the `Dockerfile` then you can automatically
## publish all declared ports to random host ports with `-P` as

## ```
## $ docker run --rm -P gplates/gws
## ```

## or with `stevedore` by using `ports = TRUE`

gws <- docker$container$run("gplates/gws", ports = TRUE, detach = TRUE)
gws$ports()

##+ include = FALSE
gws$kill()

## <small>As contributed by [Brian O'Meara](https://github.com/bomeara) in
## [#44](https://github.com/richfitz/stevedore/issues/44)</small>
