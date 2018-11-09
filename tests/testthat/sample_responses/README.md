# Sample responses

These provide integration tests for the docker responses.  The idea is to provide curated json responses from the docker daemon to particular responses.  The comments at the top of the file are in json-like format (within an R comment), and then arbitrary R code that produces a single object (perhaps after intermediate computation), for example:

```r
## version: v1.29
## method: get
## path: /_ping
## code: 200
## response: "OK"
"OK"
```

The `version`, `method`, `path`, `code` and `response` tags are all mandatory:

* `version`: docker API version (`v1.xx`)
* `method`: http method (`get`, `post`, `put`, ...)
* `path`: api path
* `code`: http response code (e.g., `200`, `202`, ...)
* `response`: response in the expected type.  Do not quote strings unless they are json strings (but the primary focus of this code is testing json endpoints)

Comments after the first non-comment line are not parsed as headers.

The response string (`response`) can get very long but it _must_ be preserved as a single line.  This is obviously not very readable.

Generate response strings with (for example)

```r
read_sample_response_str("get", "/volumes/{name}", 200, spec_1_29)
```

Or generate whole files with

``` r
add_sample_response("sample_responses/system_df.R", "get", "/system/df",
                    "200", "1.29")
```

When porting to a new version of the spec, the simplest thing to is to run:

``` r
create_sample_responses("1.99", "1.98")
```

which will create a new set of responses for version `1.99` by:
* cloning the data for `1.98` and then adding the new JSON responses from the spec
* set up testing by creating a file like `test-spec-responses-1.30.R` for the new version
* iterate through testing these while patching the [spec](../../../inst/spec/patch.yaml) and the expected responses, as required

For a failure in `v1.xx` path `foo_bar` try

```r
dat <- read_sample_response("sample_responses/v1.31/foo_bar.R")
ans1 <- dat$handler(dat$response, FALSE)
all.equal(ans1, dat$reference)
```

## Adding a new set

The easy bit; to generate responses for `1.39`, from `1.38`, do:

```
./generate.sh 1.38 1.39
```

Then, go to the spec page, e.g. https://docs.docker.com/engine/api/v1.39/

Open rewrite.md

For each endpoint in rewrite.md, identify the docker endpoint and copy the response sample into a json file.

Edit `tests/testthat/helper-stevedore.R` and lower the max version in `audit_spec_response` to your last good version

Run the tests:

```
test_file("test-spec-responses-1.39.R")
```

Inspect any failures, fix, repeat

Things to try:

* `inst/spec/patch.yml` - things that are patched up to the previous version *probably* still apply to the new version
* `git diff --no-index v1.38.yaml v1.39.yaml` gives a nice summary of changes

Tickling individual failures is helpful:

```r
dat <- read_sample_response("sample_responses/v1.39/system_df.R")
opts <- list(as_is_names = FALSE, data_frame = identity)
ans <- dat$handler(dat$response, opts)
ref <- dat$reference
all.equal(ans, ref)
```
