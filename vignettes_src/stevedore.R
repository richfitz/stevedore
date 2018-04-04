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

## `stevedore` is an R package for interacting with docker from R.
## With `stevedore` you can
##
## * create, run, manage, remove and destroy containers
## * build, pull, manage and remove images
## * create, destroy and interact with docker networks
## * create, destroy and interact with docker volumes
##
## Almost everything that can be done with the docker command line
## client (the main exception being that is not possible to send input
## to a container as if you were on a terminal).

## `stevedore` directly interacts with the docker server over its HTTP
## API.  This gives a more direct access to docker than going via the
## command line.

## This vignette quickly walks through the core features of the
## package.  Because `stevedore` wraps the docker API directly there
## are many more arguments that can be used than are covered here.
## But this covers the core use.

##+ include = FALSE
knitr::opts_chunk$set(error = FALSE)
lang_output <- function(x, lang) {
  cat(c(sprintf("```%s", lang), x, "```"), sep="\n")
}
c_output <- function(x) lang_output(x, "cc")
r_output <- function(x) lang_output(x, "r")
plain_output <- function(x) lang_output(x, "plain")
##+ include = FALSE
local({
  p <- system.file("images/iterate", package = "stevedore", mustWork = TRUE)
  stopifnot(file.copy(p, ".", recursive = TRUE))
  stevedore::docker_client()$image$build("iterate", tag = "richfitz/iterate")
})
nginx_ready <- function(port, attempts = 10) {

  f <- function() {
    url <- sprintf("http://localhost:%s", port)
    curl::parse_headers(curl::curl_fetch_memory(url)$headers)
    TRUE
  }
  for (i in seq_len(attempts)) {
    if (tryCatch(f(), error = function(e) FALSE)) {
      return()
    }
    Sys.sleep(1)
  }
  stop("nginx not available in time")
}


### TODO: A constant challenge here is printing large output in a way
### that doesn't look awful.  tibble might help, but I am not
### returning tibble's by default!  In general nested data plus very
### long ids don't really help us.

## The main function in the package is `docker_client`; this will
## construct an object with which we can talk with the docker server.
## Currently this is hardcoded to only work with a docker server that
## is running over the default unix socket (`/var/run/docker.sock`),
## but hopefully soon this can be relaxed.
docker <- stevedore::docker_client()

## The client object looks a lot like an
## [`R6`](https://github.com/wch/R6) object (though it is implemented
## differently because the interface here is automatically generated
## in ways that don't play nicely with R6).  But if you are at all
## familiar with R6 objects it should seem quite familiar.
docker

## Each function call (e.g., `ping`) is callable by accessing with
## `$`, such as
docker$ping()

## In addition there are "collection" objects (e.g., `container`)
## that are accessed using `$`, like a directory structure
docker$container

## The interface is designed similarly to the command line docker
## client (and to the Python docker client), where container commands
## are within the `container` collection and image commands are within
## the `image` collection and so on (the main difference with the
## command line client is that the container commands are not at the
## top level, so it is `docker$container$run(...)` not
## `docker$run(...)`).

## To run a container, the `docker$container$run` command follows the
## semantics of the command line client and will
##
## * pull a image if it does not exist
## * create the container
## * run the container
## * fetch the logs
res <- docker$container$run("hello-world")

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
img <- res$container$image()
img

## which is another object with methods that can be invoked to find
## out about the image, e.g.:
img$history()

## ## Containers

## The `$container` object includes methods for interacting with
## containers:
docker$container

## `$create` creates a new container (similar to `docker container
## create` on the command line) but does not start it.
x <- docker$container$create("hello-world", name = "hello-stevedore")
x

## `$get` creates a `docker_container` object from an container id or
## name:
y <- docker$container$get("hello-stevedore")
x$id()
y$id()

## `$list()` lists containers (like `docker list` on the command line)
## - by default showing only running containers
docker$container$list()
docker$container$list(all = TRUE, limit = 2)

## `$remove()` removes a container by name or id:
docker$container$remove("hello-stevedore")

## `$prune()` removes non-running containers (i.e., containers that
## have exited or containers that have been created but not yet
## started)
docker$container$prune()

## ### Working with containers

## After creating a container object, there are many more methods to
## use - all apply to the individual container
x <- docker$container$create("richfitz/iterate", c("1000", "1"))

## Most are analogues of similarly named `docker` command line
## functions.

## First, there are some basic query methods - `$id()`, `$name()` and
## `$labels()`
x$id()
x$name()
x$labels()

## More detailed information (**much** more detailed) can be reteieved
## with the `$inspect()` method
x$inspect()

## The image used by a container can be retrieved with the `$image()`
## method
x$image()

## (see below for working with images).

## The status of the container (`created`, `running`, `exited`,
## `paused`, etc) can be read with `$status()`
x$status()

## The container created by by `$create` is not running - the
## `$start()` method will start it:
x$start()
x$status()

## Once the container is running we can query to see what processes
## are running in it with `$top` (standing for Table Of Processes)
x$top()

## We can also get the logs:
x$logs()

## This returns a special object type `docker_stream` which allows
## control over formatting with `format()` - the `style` argument
## controls how stderr and stdout are printed.  There is a `stream`
## attribute that can be used to separate out lines too.  If a tty was
## allocated with `tty = TRUE` the output will be a plain character
## vector
format(x$logs(), style = "plain")

## It can generally be treated as a character vector:
x$logs()[1:2]

## The `$logs()` method can be used to do a blocking wait on a
## container.  Pass `follow = TRUE` to follow the logs.  You will want
## to provide a `stream` argument too, which is where to stream the
## log to.  This can be `stdout()` or `stderr()`, a file or an R
## connection.
y <- docker$container$create("richfitz/iterate", c("10", "0.1"))
y$start()
y$logs(stream = stdout(), follow = TRUE)

## If running this interactively, the logs will print one line at a
## time - once control returns to R the container has exited.  You can
## escape this streaming using whatever method you use to interrupt an
## R calculation (depends on which GUI/IDE you are using) but the
## container will continue regardless - we are just _observing_ a
## running container.
y$status()

## The other way of blocking until a container has finished is with
## `$wait()` which blocks until the container exits, then returns the
## exit code.
y$start()
y$wait()

## Calling `$wait()` on an exited container is fine, and will just
## return immediately:
y$wait()

## Containers can be paused with `$pause()`
x$pause()
x$status()

## Once paused, they can be restarted with `$unpause()`
x$unpause()
x$status()

## Additionally, containers can be _restarted_ with `$restart()`
x$restart(t = 0)
x$logs(tail = 5)

## Containers can be stopped with `$stop()` and removed with
## `$remove()` (calling `$remove(force = TRUE)` will kill the
## container before removing.
x$stop(t = 0)
x$remove()

## Once a container has been removed most methods will not work properly:
##+ error = TRUE
x$status()

## Information about ports (for containers that expose them) can be
## retrieved with `$ports()`.  The `nginx` image creates a web
## server/proxy that exposes port 80 from the container.  We can map
## that to a random port by asking docker to expose port 80 but not
## saying what to map it to:
nginx <- docker$container$run("nginx", ports = 80,
                              detach = TRUE, rm = TRUE,
                              name = "stevedore-nginx")
nginx$ports()

## (alternatively, use `ports = TRUE` to act like `docker run`'s `-P`
## and "publish all ports to random ports).

##+ include=FALSE
nginx_ready(nginx$ports()$host_port)

## This shows that the port exposed by the the container (80) is
## mapped to the port `r as.integer(nginx$ports()$host_port)` on the
## host.  We can use this to communicate with the server:
url <- sprintf("http://localhost:%s", nginx$ports()$host_port)
curl::parse_headers(curl::curl_fetch_memory(url)$headers)
nginx$stop(t = 0)


## ## Images

## ### Pulling

##+ include = FALSE, error = FALSE
docker$image$remove("bash:latest")

## Images can be directly pulled with `docker$image$pull` providing
## an image name (as either `<repo>` or `<repo>:<tag>`.  If the image
## exists already this will be quick, and if the network connection is
## down then this will fail.
docker$image$pull("bash:latest")

## ### The object returned by pull is an image object - that can be
## ### created by using `$get`
img <- docker$image$get("bash:latest")

## ### Building

## The other common way of getting images is to build them (the
## equivalent of `docker build`).  So if we have a path (here,
## `iterate`) containing a Dockerfile:
dir("iterate")

## The Dockerfile itself contains:
##+ echo = FALSE, results = "asis"
plain_output(readLines("iterate/Dockerfile"))

## and the `iterate` file is an executable shell script containing:
##+ echo = FALSE, results = "asis"
lang_output(readLines("iterate/iterate"), "shell")

## We can build this image using:
img <- docker$image$build("iterate", tag = "richfitz/iterate", nocache = TRUE)
img

## The newly created image is returned as an image object and can be
## used via `$container$run()`
invisible(
  docker$container$run(img, c("10", "0.1"), rm = TRUE, stream = stdout()))

##+ include = FALSE
unlink("iterate", recursive = TRUE)

## ### Importing

## There is a third way of creating an image, which is to import it
## from a tar archive.  This is not yet documented (**TODO**) but can
## be done via `$image$import()`

## ### Working with image objects

## Eachimage object has a number of methods.
img <- docker$image$get("richfitz/iterate")
img

## The `$id()`, `$short_id()`, `$labels()`, `$name()` and `$tags()`
## query basic information about an image
img$id()
img$short_id()
img$labels()
img$name()
img$tags()

## (short_id` is always 10 characters long and does not include a
## leading `sha256:`).

## The `inspect()` method returns detailed information about the image
img$inspect()

## the exact format varies between docker API versions but should be
## the same for all images within an API verison.

## The `history()` method returns a data.frame of information about
## the history of an image (i.e., the layers that it is constructed
## from)
img$history()

## (printing these objects is a real challenge!).

## The `export()` method exports an image as a tar object.  There is
## some work still required to make this work nicely (currently it
## returns a [potentially long] raw vector).

## There are several methods that operate to modify or destroy the image:

## The `$tag()` method will tag an image, for example
img$tag("richfitz/iterate", "0.0.1")
img$reload()
img$tags()

## While the `$untag()` method will remove a tag
img$untag("richfitz/iterate:0.0.1")

## The `$remove()` method will remove an image - this returns a
## data.frame indicating what actually happened (images are only
## actually deleted if there are no other tags pointing at an image
## _and_ if `noprune` is not `TRUE`.
img$remove()

## ## Volumes

## Docker volumes provide a useful abstraction for interacting with
## (possibly persistent) file volumes across containers.  To create a
## volume using `stevedore` (equivalent to `docker volume create`) use
## `$volume$create()`:
vol <- docker$volume$create("myvolume")

## Volumes can be listed:
docker$volume$list()

## ### Working with volume objects

## There's very little that can be done with volume objets:
vol

## We can get the name:
vol$name()

## Inspect the metadata
vol$inspect()

## Generate mount definitions:
vol$map("/comtainer/path")

## and can remove the volume
vol$remove()

## everything else comes from using volumes with containers.

## Containers are mounted the same way as from the docker command line
## - with a string in the form `<host>:<container>`.  Here we can use
## the volume name on the host side, so saying `<myvolume>:/myvolume`
## mounts our volume at `/myvolume` within the container.  This can be
## done easily with the `$map()` method.
vol <- docker$volume$create("myvolume")
docker$container$run(
  "alpine:latest",
  c("sh", "-c", "echo hello world > /myvolume/output"),
  volumes = vol$map("/myvolume"),
  rm = TRUE)

## (We use `sh -c` here so that the redirect operates within the
## container - the third argument is evaluated by the shell within the
## container and redirects the value that is echoed to a file.)

## We can see the result of this by using a second container to read
## the file:
docker$container$run(
  "alpine:latest",
  c("cat", "/myvolume/output"),
  volumes = vol$map("/myvolume"),
  rm = TRUE)$logs

##+ include = FALSE
vol$remove()

## ## Networks

## Docker "networks" make it easy to get containers communicating with
## each other without exposing ports to the host.  To achive this, one
## creates a docker network, then create containers attached to that
## network (containers can also be attached to networks after
## creation).
nw <- docker$network$create("mynetwork")

## Networks can be listed:
docker$network$list()

## The networks `bridge`, `host` and `none` always exist - they are
## special to docker.

## ### Working with network objects

## Like volume objects, network objects do very little themselves:
nw

## The `$name()` and `$id()` methods get the name and id of the network
nw$name()
nw$id()

## `$inspect()` gets detailed metadata
nw$inspect()

## `$containers()` lists containers attached to the network (currently
## an empty list - this network has no attached containers)
nw$containers()

## `$remove()` removes the network
nw$remove()

## Generally you'll want to put containers onto a network.

## The setup here is to create a network, and then use the `network`
## argument to `$container$run()` to attach a container to that
## network.  Once established, containers on the same network can use
## another docker container's name as the hostname and communicate!
nw <- docker$network$create("mynetwork")
server <- docker$container$run("nginx", network = nw, name = "server",
                               detach = TRUE, rm = TRUE)
server$status()

## Now we can attach other networks to this container and communicate
## with the server:
docker$container$run("alpine:latest", c("ping", "server", "-c", "3"),
                     network = nw, stream = stdout(), rm = TRUE)

## Omitting the `network` argument, the second container can't find
## the server:
##+ error = TRUE
docker$container$run("alpine:latest", c("ping", "server", "-c", "3"),
                     stream = stdout(), rm = TRUE)

## The server container exposes a webserver on port 80.  For
## containers on the network we can access this port:
res <- docker$container$run("richfitz/curl", c("curl", "-s", "http://server"),
                            network = nw, rm = TRUE)
head(res$logs, 10)

server$stop()
nw$remove()

## ## Other docker functions

## There are a few functions at the top level of the `docker_client`
## object:

## `$ping()` tests the connection to the server and reports the API
## version for the server - this is a (for docker) very fast function
## to use to test that things seem to be working.
docker$ping()

## `$api_version()` reports the API version that the *client* is using
## (this can be varied from `r stevedore:::DOCKER_API_VERSION_MIN` to
## `r stevedore:::DOCKER_API_VERSION_MAX`)
docker$api_version()

## `$version()` reports detailed version information from the server:
docker$version()

## `$info()` reports a bunch of other information about the state of
## the server (Docker describes this as "get system information" in
## its documentation) - this is equivalent to running `docker info`
docker$info()

## Finally, `$df()` will return information about resource and data
## usage by docker - all containers, networks, volumes, etc.
