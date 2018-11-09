## version: 1.39
## method: get
## path: /info
## code: 200
registry_config <- list(
  allow_nondistributable_artifacts_cidrs = c("::1/128", "127.0.0.0/8"),
  allow_nondistributable_artifacts_hostnames = c(
    "registry.internal.corp.example.com:3000", "[2001:db8:a0b:12f0::1]:443"),
  insecure_registry_cidrs = c("::1/128", "127.0.0.0/8"),
  index_configs = list(
    "127.0.0.1:5000" = list(
      name = "127.0.0.1:5000",
      mirrors = character(0),
      secure = FALSE,
      official = FALSE),
    "[2001:db8:a0b:12f0::1]:80" = list(
      name = "[2001:db8:a0b:12f0::1]:80",
      mirrors = character(0),
      secure = FALSE,
      official = FALSE),
    "docker.io" = list(
      name = "docker.io",
      mirrors = "https://hub-mirror.corp.example.com:5000/",
      secure = TRUE,
      official = TRUE),
    "registry.internal.corp.example.com:3000" = list(
      name = "registry.internal.corp.example.com:3000",
      mirrors = character(0),
      secure = FALSE,
      official = FALSE)),
  mirrors= c(
    "https://hub-mirror.corp.example.com:5000/",
    "https://[2001:db8:a0b:12f0::1]/"))

system_status <- list(
  c("Role", "primary"),
  c("State", "Healthy"),
  c("Strategy", "spread"),
  c("Filters", "health, port, containerslots, dependency, affinity, constraint, whitelist"),
  c("Nodes", "2"),
  c(" swarm-agent-00", "192.168.99.102:2376"),
  c("  └ ID", "5CT6:FBGO:RVGO:CZL4:PB2K:WCYN:2JSV:KSHH:GGFW:QOPG:6J5Q:IOZ2|192.168.99.102:2376"),
  c("  └ Status", "Healthy"),
  c("  └ Containers", "1 (1 Running, 0 Paused, 0 Stopped)"),
  c("  └ Reserved CPUs", "0 / 1"),
  c("  └ Reserved Memory", "0 B / 1.021 GiB"),
  c("  └ Labels", "kernelversion=4.4.74-boot2docker, operatingsystem=Boot2Docker 17.06.0-ce (TCL 7.2); HEAD : 0672754 - Thu Jun 29 00:06:31 UTC 2017, ostype=linux, provider=virtualbox, storagedriver=aufs"),
  c("  └ UpdatedAt", "2017-08-09T10:03:46Z"),
  c("  └ ServerVersion", "17.06.0-ce"),
  c(" swarm-manager", "192.168.99.101:2376"),
  c("  └ ID", "TAMD:7LL3:SEF7:LW2W:4Q2X:WVFH:RTXX:JSYS:XY2P:JEHL:ZMJK:JGIW|192.168.99.101:2376"),
  c("  └ Status", "Healthy"),
  c("  └ Containers", "2 (2 Running, 0 Paused, 0 Stopped)"),
  c("  └ Reserved CPUs", "0 / 1"),
  c("  └ Reserved Memory", "0 B / 1.021 GiB"),
  c("  └ Labels", "kernelversion=4.4.74-boot2docker, operatingsystem=Boot2Docker 17.06.0-ce (TCL 7.2); HEAD : 0672754 - Thu Jun 29 00:06:31 UTC 2017, ostype=linux, provider=virtualbox, storagedriver=aufs"),
  c("  └ UpdatedAt", "2017-08-09T10:04:11Z"),
  c("  └ ServerVersion", "17.06.0-ce"))

tls_info <- list(
  trust_root = "-----BEGIN CERTIFICATE-----\nMIIBajCCARCgAwIBAgIUbYqrLSOSQHoxD8CwG6Bi2PJi9c8wCgYIKoZIzj0EAwIw\nEzERMA8GA1UEAxMIc3dhcm0tY2EwHhcNMTcwNDI0MjE0MzAwWhcNMzcwNDE5MjE0\nMzAwWjATMREwDwYDVQQDEwhzd2FybS1jYTBZMBMGByqGSM49AgEGCCqGSM49AwEH\nA0IABJk/VyMPYdaqDXJb/VXh5n/1Yuv7iNrxV3Qb3l06XD46seovcDWs3IZNV1lf\n3Skyr0ofcchipoiHkXBODojJydSjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMB\nAf8EBTADAQH/MB0GA1UdDgQWBBRUXxuRcnFjDfR/RIAUQab8ZV/n4jAKBggqhkjO\nPQQDAgNIADBFAiAy+JTe6Uc3KyLCMiqGl2GyWGQqQDEcO3/YG36x7om65AIhAJvz\npxv6zFeVEkAEEkqIYi0omA9+CjanB/6Bz4n1uw8H\n-----END CERTIFICATE-----\n",
  cert_issuer_subject = "MBMxETAPBgNVBAMTCHN3YXJtLWNh",
  cert_issuer_public_key = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEmT9XIw9h1qoNclv9VeHmf/Vi6/uI2vFXdBveXTpcPjqx6i9wNazchk1XWV/dKTKvSh9xyGKmiIeRcE4OiMnJ1A==")

swarm_cluster <- list(
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
  root_rotation_in_progress = FALSE)

swarm <- list(
  node_id = "k67qz4598weg5unwwffg6z1m1",
  node_addr = "10.0.0.46",
  local_node_state = "active",
  control_available = TRUE,
  error = "",
  remote_managers = data_frame(
    node_id = c("71izy0goik036k48jg985xnds", "79y6h1o4gv8n120drcprv5nmc",
                "k67qz4598weg5unwwffg6z1m1"),
    addr = c("10.0.0.158:2377", "10.0.0.159:2377", "10.0.0.46:2377")),
  nodes = 4L,
  managers = 3L,
  cluster = swarm_cluster)

generic_resources <- assigned_generic_resources <- data_frame(
  named_resource_spec = I(list(
    list(kind = NA_character_, value = NA_character_),
    list(kind = "GPU", value = "UUID1"),
    list(kind = "GPU", value = "UUID2"))),
  discrete_resource_spec = I(list(
    list(kind = "SSD", value = 3L),
    list(kind = NA_character_, value = NA_integer_),
    list(kind = NA_character_, value = NA_integer_))))

list(
  id = "7TRN:IPZB:QYBB:VPBQ:UMPP:KARE:6ZNR:XE6T:7EWV:PKF4:ZOJD:TPYS",
  containers = 14L,
  containers_running = 3L,
  containers_paused = 1L,
  containers_stopped = 10L,
  images = 508L,
  driver = "overlay2",
  driver_status = list(c("Backing Filesystem", "extfs"),
                       c("Supports d_type", "true"),
                       c("Native Overlay Diff", "true")),
  docker_root_dir = "/var/lib/docker",
  system_status = system_status,
  plugins = list(
    volume = "local",
    network = c("bridge", "host", "ipvlan", "macvlan", "null", "overlay"),
    authorization = c("img-authz-plugin", "hbm"),
    log = c("awslogs", "fluentd", "gcplogs", "gelf", "journald", "json-file",
            "logentries", "splunk", "syslog")),
  memory_limit = TRUE,
  swap_limit = TRUE,
  kernel_memory = TRUE,
  cpu_cfs_period = TRUE,
  cpu_cfs_quota = TRUE,
  cpu_shares = TRUE,
  cpu_set = TRUE,
  oom_kill_disable = TRUE,
  ipv4_forwarding = TRUE,
  bridge_nf_iptables = TRUE,
  bridge_nf_ip6tables = TRUE,
  debug = TRUE,
  n_fd = 64L,
  n_goroutines = 174L,
  system_time = "2017-08-08T20:28:29.06202363Z",
  logging_driver = "string",
  cgroup_driver = "cgroupfs",
  n_events_listener = 30L,
  kernel_version = "4.9.38-moby",
  operating_system = "Alpine Linux v3.5",
  os_type = "linux",
  architecture = "x86_64",
  n_cpu = 4L,
  mem_total = 2095882240,
  index_server_address = "https://index.docker.io/v1/",
  registry_config = registry_config,
  generic_resources = generic_resources,
  http_proxy = "http://xxxxx:xxxxx@proxy.corp.example.com:8080",
  https_proxy = "https://xxxxx:xxxxx@proxy.corp.example.com:4443",
  no_proxy = "*.local, 169.254/16",
  name = "node5.corp.example.com",
  labels = c("storage=ssd", "production"),
  experimental_build = TRUE,
  server_version = "17.06.0-ce",
  cluster_store = "consul://consul.corp.example.com:8600/some/path",
  cluster_advertise = "node5.corp.example.com:8000",
  runtimes = list(
    runc = list(path = "runc", runtime_args = character()),
    "runc-master" = list(path = "/go/bin/runc", runtime_args = character()),
    custom = list(path = "/usr/local/bin/my-oci-runtime",
                  runtime_args = c("--debug", "--systemd-cgroup=false"))),
  default_runtime = "runc",
  swarm = swarm,
  live_restore_enabled = FALSE,
  isolation = "default",
  init_binary = "docker-init",
  containerd_commit = list(
    id = "cfb82a876ecc11b5ca0977d1733adbe58599088a",
    expected = "2d41c047c83e09a6d61d464906feb2a2f3c52aa4"),
  runc_commit = list(
    id = "cfb82a876ecc11b5ca0977d1733adbe58599088a",
    expected = "2d41c047c83e09a6d61d464906feb2a2f3c52aa4"),
  init_commit = list(
    id = "cfb82a876ecc11b5ca0977d1733adbe58599088a",
    expected = "2d41c047c83e09a6d61d464906feb2a2f3c52aa4"),
  security_options = c(
    "name=apparmor", "name=seccomp,profile=default", "name=selinux",
    "name=userns"),
  product_license = "Community Engine",
  warnings = c(
    "WARNING: No memory limit support",
    "WARNING: bridge-nf-call-iptables is disabled",
    "WARNING: bridge-nf-call-ip6tables is disabled"))
