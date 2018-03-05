Notes on developing `stevedore` - read this before trying to get started changing it!

## Running tests

By default, `stevedore` will not run any test that interacts with the docker daemon, and only test "safe" code.  You must opt-in to the full test suite by setting the environment variable `STEVEDORE_TEST_USE_DOCKER` to `true`.

**WARNING**: If you do opt in to the full test suite, it will do all sorts of things to your containers/images/volumes/networks.  Do not run it unless you'd be happy running `docker system prune -f --volumes`.  It will not warn before deleting anything and **will** call all `prune` commands during running.  The docker-using tests are also much slower to run than the offline tests (~100s vs 16s).

There are some tests that authenticate to dockerhub to use a private repository.  These tests will just skip if the `STEVEDORE_STEVEDOREBOT_PASS` environment variable is not set to the password.  If this poses a problem, this can be generalised out to use any account.

## Design notes

### `docker_client`

At the outermost level, we have `docker_client` - this is the core object for user interaction.  This is the main entrypoint in the package.  It is composed of `stevedore_object` elements which control printing, access etc. They are like very simple versions of R6 objects.

Implementation is spread across:

  - `docker_client.R` contains the implementation of the objects
  - `docker_client_support.R` contains support functions to make that work
  - `docker_client_run.R` container the `docker$containers$run` function, which is by far the most complex
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
