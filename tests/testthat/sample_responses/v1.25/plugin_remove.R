## version: 1.25
## method: delete
## path: /plugins/{name}
## code: 200
## response: ~
NULL

## NOTE: The spec says that with a "200 no error" response it will
## return `#/definitions/Plugin`, but empirically it produces a null
## response (which is more consistent with other endpoints)
NULL
