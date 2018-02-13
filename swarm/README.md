Vagrant based environment for testing docker swarm and connecting over https.

## Usage with https

You only need the manager up

```
vagrant up manager
```

This will set up docker, and configure it to allow access over https:

```
docker -H=127.0.0.1:2376 \
    --tlsverify \
    --tlscacert=shared/ssl/ca.pem \
    --tlscert=shared/ssl/cert.pem \
    --tlskey=shared/ssl/key.pem \
    version
```

Or, using curl:

```
curl https://127.0.0.1:2376/v1.29/version --verbose -iv \
  --cert shared/ssl/cert.pem \
  --key shared/ssl/key.pem \
  --cacert shared/ssl/ca.pem
```

Or, using R:

```
library(curl)
debugfun <- function(type, data){
  if(type < 5)
  cat(rawToChar(data))
}
full_path <- function(x) {
  normalizePath(x, mustWork = TRUE)
}
h <- new_handle(capath = full_path("shared/ssl/ca.pem"),
                sslcert = full_path("shared/ssl/cert.pem"),
                sslkey = full_path("shared/ssl/key.pem"),
                verbose = TRUE,
                debugfunction = debugfun,
                ssl_verifystatus = FALSE)
curl_fetch_memory("https://127.0.0.1:2376/v1.29/version", handle = h)
```
