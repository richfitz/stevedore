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
