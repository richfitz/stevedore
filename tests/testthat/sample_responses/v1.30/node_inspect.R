## version: 1.30
## method: get
## path: /nodes/{id}
## code: 200
NULL

## NOTE: the sample response for this version is incorrect, with TLS
## in the wrong place and so I've updated the json
tls_info <- list(
  trust_root = "-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n",
  cert_issuer_subject = "MBMxETAPBgNVBAMTCHN3YXJtLWNh",
  cert_issuer_public_key = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A==")


list(
  id = "24ifsmvkjbyhk",
  version = list(index = 8L),
  created_at = "2016-06-07T20:31:11.853781916Z",
  updated_at = "2016-06-07T20:31:11.999868824Z",
  spec = list(
    name = "my-node",
    labels = c(foo = "bar"),
    role = "manager",
    availability = "active"),
  description = list(
    hostname = "bf3067039e47",
    platform = list(
      architecture = "x86_64",
      os = "linux"),
    resources = list(
      nano_cpus = 4e+09,
      memory_bytes = 8272408576),
    engine = list(
      engine_version = "17.04.0",
      labels = c(foo = "bar"),
      plugins = data.frame(
        type = c("Volume", "Network", "Network", "Network"),
        name = c("local",  "bridge",  "null",    "overlay"),
        stringsAsFactors = FALSE)),
    tls_info = tls_info),
  status = list(
    state = "ready",
    message = NA_character_,
    addr = "172.17.0.2"),
  manager_status = list(
    leader = TRUE,
    reachability = "reachable",
    addr = "172.17.0.2:2377"))
