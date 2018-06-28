Notes on developing `stevedore` - read this before trying to get started changing it!

## Running tests

By default, `stevedore` will not run any test that interacts with the docker daemon, and only test "safe" code.  You must opt-in to the full test suite by setting the environment variable `STEVEDORE_TEST_USE_DOCKER` to `true`.

**WARNING**: If you do opt in to the full test suite, it will do all sorts of things to your containers/images/volumes/networks.  Do not run it unless you'd be happy running `docker system prune -f --volumes`.  It will not warn before deleting anything and **will** call all `prune` commands during running.  The docker-using tests are also much slower to run than the offline tests (~100s vs 16s).

There are some tests that authenticate to dockerhub to use a private repository.  These tests will just skip if the `STEVEDORE_STEVEDOREBOT_PASS` environment variable is not set to the password.  If this poses a problem, this can be generalised out to use any account.

There are tests that use a `docker-machine` instance.  To enable these, `STEVEDORE_TEST_USE_DOCKER` must be `true` **and** the environment variable `STEVEDORE_TEST_DOCKER_MACHINE_NAME` must be set to the name of a `docker-machine` machine.  That machine must be running (so that `docker-machine env $STEVEDORE_TEST_DOCKER_MACHINE_NAME` works).

The test behaviour is further modified by:

- `STEVEDORE_TEST_REQUIRE_USE_DOCKER` if set to `true`, then failure to create a docker client is an error
- `STEVEDORE_TEST_STRICT_CLEANUP` if set to `true`, then the preflight checks are done at every docker client creation, which will make it easier to detect when tests are leaving behind orphan containers, networks, volumes, etc.

## Design notes

### `docker_client`

At the outermost level, we have `docker_client` - this is the core object for user interaction.  This is the main entrypoint in the package.  It is composed of `stevedore_object` elements which control printing, access etc. They are like very simple versions of R6 objects.

Implementation is spread across:

  - `docker_client.R` contains the implementation of the objects
  - `docker_client_support.R` contains support functions to make that work
  - `docker_client_run.R` container the `docker$container$run` function, which is by far the most complex
  - `docker_client_method.R` defines how methods for the docker client are implemented

### `docker_api_client`

The `docker_client` object does communication via an `docker_api_client` object.  This creates a mapping between docker's endpoints and R functions that can be called.  The `docker_api_client` object contains two objects - `http_client` which contains the transport support and `endpoints`, which has information on each endpoint.

These are put together with the `run_endpoint` function which takes the `http_client` object, a single endpoint from the `endpoints` list, and a set of parameters.  It will perform the request and unserialise the returned (probably json) object into something suitable for R.

### `docker_http_client`

This is just a function that performs a request against a docker server, abstracting away what protocol this over and returning a consistent set of elements (a subest of what `curl::curl_fetch_memory` returns).

The flow here is a bit complicated and is easiest to visualise with a specific example.

``` yaml
  /containers/{id}:
    delete:
      summary: "Remove a container"
      operationId: "ContainerDelete"
      responses:
        204:
          description: "no error"
        404:
          description: "no such container"
      parameters:
        - name: "id"
          in: "path"
          required: true
          description: "ID or name of the container"
          type: "string"
        - name: "v"
          in: "query"
          description: "Remove the volumes associated with the container."
          type: "boolean"
          default: false
        - name: "force"
          in: "query"
          description: "If the container is running, kill it before removing it."
          type: "boolean"
          default: false
        - name: "link"
          in: "query"
          description: "Remove the specified link associated with the container."
          type: "boolean"
          default: false
```

To handle this request we need to collect up parameters `id`, `v`, `force` and `link`, construct a valid URL and send a http `DELETE` request to that url.  If the

(which at the R level are all "equivalent" - it doesn't matter that some are query parameters and others are

  - `docker_api_client.R` defines the main object.  It has two objects - `http_client` which contains the transport support and `endpoints`, which has information on each endpoint.
  - The `run_endpoint` function takes this `http_client` and one of the endpoint object, along with any parameters (a list with elements `path`, `query`, `body` and `header`) and performs the request

## Notes

### The names cache

```r
devtools::load_all()
names <- update_name_cache(".")
```

check for possible special cases here:

```r
tmp <- names[grepl("^[a-z]_", names[, 2L]), ]
```

Update the tests in test-util.R

```r
tmp <- names[grepl("^[A-Z]{2}", names[, 1L]), ]
writeLines(sprintf('  expect_equal(pascal_to_snake("%s"), "%s")',
                   tmp[, 1L], tmp[, 2L]))
```

### Debugging TLS connections

```r
library(curl)
debugfun <- function(type, data){
  if (type < 5) {
    cat(rawToChar(data))
  }
}
h <- new_handle(capath = "ca.pem",
                sslcert = "cert.pem",
                sslkey = "key.pem",
                verbose = TRUE,
                debugfunction = debugfun,
                ssl_verifystatus = FALSE)
curl_fetch_memory("https://127.0.0.1:2376/v1.29/version", handle = h)
```

### Debugging HTTP requests

From the `stevedore` client itself, get debugging information as

```r
cl <- stevedore::docker_client(debug = TRUE)
```

which will print all chatter that the client sends and recieves.

To see everything that happens across the server, first set up a proxy with:

```
docker run --rm \
       -v /var/run/docker.sock:/var/run/docker.sock \
       -p 2375:2375 \
       --name docker-proxy \
       bobrik/socat \
       -v TCP4-LISTEN:2375,fork,reuseaddr UNIX-CONNECT:/var/run/docker.sock
```

Then do (in a second terminal)

```
DOCKER_HOST=tcp://localhost:2375 docker version
```

and see the http requests made by the official client (will appear in the first terminal), and


```r
cl <- stevedore::docker_client(host = "tcp://localhost:2375")
cl$version()
```

to see the requests made by `stevedore`.
