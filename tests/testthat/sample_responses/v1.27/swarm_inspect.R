## version: 1.27
## method: get
## path: /swarm
## code: 200
## response: {"CreatedAt":"2016-08-15T16:00:20.349727406Z","Spec":{"Dispatcher":{"HeartbeatPeriod":5000000000},"Orchestration":{"TaskHistoryRetentionLimit":10},"CAConfig":{"NodeCertExpiry":7.776e+15},"Raft":{"LogEntriesForSlowFollowers":500,"HeartbeatTick":1,"SnapshotInterval":10000,"ElectionTick":3},"TaskDefaults":{},"EncryptionConfig":{"AutoLockManagers":false},"Name":"default"},"JoinTokens":{"Worker":"SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-6qmn92w6bu3jdvnglku58u11a","Manager":"SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-8llk83c4wm9lwioey2s316r9l"},"ID":"70ilmkj2f6sp2137c753w2nmt","UpdatedAt":"2016-08-15T16:32:09.623207604Z","Version":{"Index":51}}
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
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
        options = I(list()))),
    encryption_config = list(auto_lock_managers = FALSE),
    task_defaults = list(log_driver = NULL)),
  join_tokens = list(
    worker = "SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-6qmn92w6bu3jdvnglku58u11a",
    manager = "SWMTKN-1-1h8aps2yszaiqmz2l3oc5392pgk8e49qhx2aj3nyv0ui0hez2a-8llk83c4wm9lwioey2s316r9l"))
