## version: 1.31
## method: get
## path: /info
## code: 200
index_config <- list(
  "127.0.0.1:5000" = list(
    mirrors = character(0),
    name = "127.0.0.1:5000",
    official = FALSE,
    secure = FALSE),
  "[2001:db8:a0b:12f0::1]:80" = list(
    mirrors = character(0),
    name = "[2001:db8:a0b:12f0::1]:80",
    official = FALSE,
    secure = FALSE),
  docker.io = list(
    mirrors = "https://hub-mirror.corp.example.com:5000/",
    name = "docker.io",
    official = TRUE,
    secure = TRUE),
  "registry.internal.corp.example.com:3000" = list(
    mirrors = character(0),
    name = "registry.internal.corp.example.com:3000",
    official = FALSE,
    secure = FALSE))


registry_config <- list(
  index_configs = index_config,
  insecure_registry_cidrs = c("::1/128", "127.0.0.0/8"))


driver_status <- list(
  c("Backing Filesystem", "extfs"),
  c("Supports d_type", "true"),
  c("Native Overlay Diff", "true"))


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


list(
  architecture = "x86_64",
  containers = 14L,
  containers_running = 3L,
  containers_stopped = 10L,
  containers_paused = 1L,
  cpu_cfs_period = TRUE,
  cpu_cfs_quota = TRUE,
  debug = TRUE,
  discovery_backend = NA_character_,
  docker_root_dir = "/var/lib/docker",
  driver = "overlay2",
  driver_status = driver_status,
  system_status = system_status,
  plugins = list(volume = "local",
                 network = c("bridge", "host", "ipvlan", "macvlan",
                             "null", "overlay"),
                 log = c("awslogs", "fluentd", "gcplogs", "gelf", "journald",
                         "json-file", "logentries", "splunk", "syslog")),
  experimental_build = TRUE,
  http_proxy = "http://user:pass@proxy.corp.example.com:8080",
  https_proxy = "https://user:pass@proxy.corp.example.com:4443",
  id = "7TRN:IPZB:QYBB:VPBQ:UMPP:KARE:6ZNR:XE6T:7EWV:PKF4:ZOJD:TPYS",
  ipv4_forwarding = TRUE,
  images = 508L,
  index_server_address = "https://index.docker.io/v1/",
  init_path = NA_character_,
  init_sha1 = NA_character_,
  kernel_version = "4.9.38-moby",
  labels = c("storage=ssd", "production"),
  mem_total = 2095882240L,
  memory_limit = TRUE,
  n_cpu = 4L,
  n_events_listener = 30L,
  n_fd = 64L,
  n_goroutines = 174L,
  name = "node5.corp.example.com",
  no_proxy = "*.local, 169.254/16",
  oom_kill_disable = TRUE,
  os_type = "linux",
  oom_score_adj = NA_integer_,
  operating_system = "Alpine Linux v3.5",
  registry_config = registry_config,
  swap_limit = TRUE,
  system_time = "2017-08-08T20:28:29.06202363Z",
  server_version = "17.06.0-ce"
  )
