## version: 1.36
## method: get
## path: /nodes
## code: 200
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

tls_info <- list(
  trust_root = "-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n",
  cert_issuer_subject = "MBMxETAPBgNVBAMTCHN3YXJtLWNh",
  cert_issuer_public_key = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A==")

data_frame(
  id = "24ifsmvkjbyhk",
  version = I(list(list(index = 373531L))),
  created_at = "2016-08-18T10:44:24.496525531Z",
  updated_at = "2017-08-09T07:09:37.632105588Z",
  spec = I(list(list(
    name = "node-name",
    labels = c(foo = "bar"),
    role = "manager",
    availability = "active"))),
  description = I(list(list(
    hostname = "bf3067039e47",
    platform = list(
      architecture = "x86_64",
      os = "linux"),
    resources = list(
      nano_cpus = 4e+09,
      memory_bytes = 8272408576,
      generic_resources = data.frame(
        named_resource_spec = I(list(
          list(kind = NA_character_, value = NA_character_),
          list(kind = "GPU", value = "UUID1"),
          list(kind = "GPU", value = "UUID2"))),
        discrete_resource_spec = I(list(
          list(kind = "SSD", value = 3L),
          list(kind = NA_character_, value = NA_integer_),
          list(kind = NA_character_, value = NA_integer_))))),
    engine = list(
      engine_version = "17.06.0",
      labels = c(foo = "bar"),
      plugins = data.frame(
        type = c("Log", "Log", "Log", "Log", "Log", "Log", "Log", "Log", "Log",
                 "Network", "Network", "Network", "Network", "Network",
                 "Network", "Volume", "Volume", "Volume"),
        name = c("awslogs", "fluentd", "gcplogs", "gelf", "journald",
                 "json-file", "logentries", "splunk", "syslog", "bridge",
                 "host", "ipvlan", "macvlan", "null", "overlay", "local",
                 "localhost:5000/vieux/sshfs:latest",
                 "vieux/sshfs:latest"),
        stringsAsFactors = FALSE)),
    tls_info = tls_info))),
  status = I(list(list(
    state = "ready",
    message = "",
    addr = "172.17.0.2"))),
  manager_status = I(list(list(
    leader = TRUE,
    reachability = "reachable",
    addr = "10.0.0.46:2377"))))
