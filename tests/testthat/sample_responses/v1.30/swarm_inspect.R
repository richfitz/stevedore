## version: 1.30
## method: get
## path: /swarm
## code: 200
## response: {"CreatedAt":"2016-08-15T16:00:20.349727406Z","Spec":{"Dispatcher":{"HeartbeatPeriod":5000000000},"Orchestration":{"TaskHistoryRetentionLimit":10},"CAConfig":{"NodeCertExpiry":7.776e+15},"Raft":{"LogEntriesForSlowFollowers":500,"HeartbeatTick":1,"SnapshotInterval":10000,"ElectionTick":3},"TaskDefaults":{},"EncryptionConfig":{"AutoLockManagers":false},"Name":"default"},"JoinTokens":{"Worker":"SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-6qmn92w6bu3jdvnglku58u11a","Manager":"SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-8llk83c4wm9lwioey2s316r9l"},"ID":"70ilmkj2f6sp2137c753w2nmt","UpdatedAt":"2016-08-15T16:32:09.623207604Z","Version":{"Index":51},"RootRotationInProgress":false,"TLSInfo":{"TrustRoot":"-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n","CertIssuerSubject":"MBMxETAPBgNVBAMTCHN3YXJtLWNh","CertIssuerPublicKey":"MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A=="}}
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

tls_info <- list(
  trust_root = "-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n",
  cert_issuer_subject = "MBMxETAPBgNVBAMTCHN3YXJtLWNh",
  cert_issuer_public_key = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A==")

list(
  id = "70ilmkj2f6sp2137c753w2nmt",
  version = list(index = 51L),
  created_at = "2016-08-15T16:00:20.349727406Z",
  updated_at = "2016-08-15T16:32:09.623207604Z",
  spec = list(
    name = "default",
    labels = NULL,
    orchestration = list(task_history_retention_limit = 10L),
    raft = list(
      snapshot_interval = 10000L,
      keep_old_snapshots = NA_integer_,
      log_entries_for_slow_followers = 500L,
      election_tick = 3L,
      heartbeat_tick = 1L),
    dispatcher = list(heartbeat_period = 5e+09),
    ca_config = list(
      node_cert_expiry = 7.776e+15,
      external_cas = data_frame(
        protocol = character(0),
        url = character(0),
        options = I(list()),
        ca_cert = character(0)),
      signing_cacert = NA_character_,
      signing_cakey = NA_character_,
      force_rotate = NA_integer_),
    encryption_config = list(auto_lock_managers = FALSE),
    task_defaults = list(log_driver = NULL)),
  tls_info = tls_info,
  root_rotation_in_progress = FALSE,
  join_tokens = list(
    worker = "SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-6qmn92w6bu3jdvnglku58u11a",
    manager = "SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-8llk83c4wm9lwioey2s316r9l"))
