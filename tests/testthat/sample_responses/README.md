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
                    "200", "v1.29")
```

When porting to a new version of the spec, the simplest thing to is to run:

``` r
create_sample_responses("1.99", "1.98")
```

which will create a new set of responses for version `1.99` by:
* cloning the data for `1.98` and then adding the new JSON responses from the spec
* set up testing by creating a file like `test-spec-responses-1.30.R` for the new version
* iterate through testing these while patching the [spec](../../../inst/spec/patch.yaml) and the expected responses, as required
