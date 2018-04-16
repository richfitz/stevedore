## version: 1.37
## method: get
## path: /swarm
## code: 200
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

tls_info <- list(
  trust_root = "-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n",
  cert_issuer_subject = "MBMxETAPBgNVBAMTCHN3YXJtLWNh",
  cert_issuer_public_key = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A==")

list(
  id = "abajmipo7b4xz5ip2nrla6b11",
  version = list(index = 373531L),
  created_at = "2016-08-18T10:44:24.496525531Z",
  updated_at = "2017-08-09T07:09:37.632105588Z",
  spec = list(
    name = "default",
    labels = c(com.example.corp.type = "production",
               com.example.corp.department = "engineering"),
    orchestration = list(task_history_retention_limit = 10L),
    raft = list(
      snapshot_interval = 10000L,
      keep_old_snapshots = 0L,
      log_entries_for_slow_followers = 500L,
      election_tick = 3L,
      heartbeat_tick = 1L),
    dispatcher = list(heartbeat_period = 5e+09),
    ca_config = list(
      node_cert_expiry = 7.776e+15,
      external_cas = data_frame(
        protocol = "cfssl",
        url = "string",
        options = I(list(c(
          property1 = "string",
          property2 = "string"))),
        ca_cert = "string"),
      signing_cacert = "string",
      signing_cakey = "string",
      force_rotate = 0L),
    encryption_config = list(auto_lock_managers = FALSE),
    task_defaults = list(
      log_driver = list(
        name = "json-file",
        options = c("max-file" = "10", "max-size" = "100m")))),
  tls_info = tls_info,
  root_rotation_in_progress = FALSE,
  join_tokens = list(
    worker = "SWMTKN-1-3pu6hszjas19xyp7ghgosyx9k8atbfcr8p2is99znpy26u2lkl-1awxwuwd3z9j1z3puu7rcgdbx",
    manager = "SWMTKN-1-3pu6hszjas19xyp7ghgosyx9k8atbfcr8p2is99znpy26u2lkl-7p73s1dx5in4tatdymyhg9hu2"))
