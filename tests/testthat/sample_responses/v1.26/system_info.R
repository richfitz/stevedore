## version: 1.26
## method: get
## path: /info
## code: 200
## response: {"Architecture":"x86_64","ClusterStore":"etcd://localhost:2379","CgroupDriver":"cgroupfs","Containers":11,"ContainersRunning":7,"ContainersStopped":3,"ContainersPaused":1,"CpuCfsPeriod":true,"CpuCfsQuota":true,"Debug":false,"DockerRootDir":"/var/lib/docker","Driver":"btrfs","DriverStatus":"","ExperimentalBuild":false,"HttpProxy":"http://test:test@localhost:8080","HttpsProxy":"https://test:test@localhost:8080","ID":"7TRN:IPZB:QYBB:VPBQ:UMPP:KARE:6ZNR:XE6T:7EWV:PKF4:ZOJD:TPYS","IPv4Forwarding":true,"Images":16,"IndexServerAddress":"https://index.docker.io/v1/","InitPath":"/usr/bin/docker","InitSha1":"","KernelMemory":true,"KernelVersion":"3.12.0-1-amd64","Labels":"storage=ssd","MemTotal":2099236864,"MemoryLimit":true,"NCPU":1,"NEventsListener":0,"NFd":11,"NGoroutines":21,"Name":"prod-server-42","NoProxy":"9.81.1.160","OomKillDisable":true,"OSType":"linux","OperatingSystem":"Boot2Docker","Plugins":{"Volume":"local","Network":["null","host","bridge"]},"RegistryConfig":{"IndexConfigs":{"docker.io":{"Name":"docker.io","Official":true,"Secure":true}},"InsecureRegistryCIDRs":"127.0.0.0/8"},"SecurityOptions":[{"Key":"Name","Value":"seccomp"},{"Key":"Profile","Value":"default"},{"Key":"Name","Value":"apparmor"},{"Key":"Name","Value":"selinux"},{"Key":"Name","Value":"userns"}],"ServerVersion":"1.9.0","SwapLimit":false,"SystemStatus":"c(\"State\", \"Healthy\")","SystemTime":"2015-03-10T11:11:23.730591467-07:00"}
index_config <- list(
  docker.io = list(
    mirrors = character(0),
    name = "docker.io",
    official = TRUE,
    secure = TRUE))

registry_config <- list(
  index_configs = index_config,
  insecure_registry_cidrs = "127.0.0.0/8"
)

list(
  architecture = "x86_64",
  containers = 11L,
  containers_running = 7L,
  containers_stopped = 3L,
  containers_paused = 1L,
  cpu_cfs_period = TRUE,
  cpu_cfs_quota = TRUE,
  debug = FALSE,
  discovery_backend = NA_character_,
  docker_root_dir = "/var/lib/docker",
  driver = "btrfs",
  driver_status = list(""),
  system_status = list("c(\"State\", \"Healthy\")"),
  plugins = list(volume = "local",
                 network = c("null", "host", "bridge")),
  experimental_build = FALSE,
  http_proxy = "http://test:test@localhost:8080",
  https_proxy = "https://test:test@localhost:8080",
  id = "7TRN:IPZB:QYBB:VPBQ:UMPP:KARE:6ZNR:XE6T:7EWV:PKF4:ZOJD:TPYS",
  ipv4_forwarding = TRUE,
  images = 16L,
  index_server_address = "https://index.docker.io/v1/",
  init_path = "/usr/bin/docker",
  init_sha1 = "",
  kernel_version = "3.12.0-1-amd64",
  labels = "storage=ssd",
  mem_total = 2099236864L,
  memory_limit = TRUE,
  n_cpu = 1L,
  n_events_listener = 0L,
  n_fd = 11L,
  n_goroutines = 21L,
  name = "prod-server-42",
  no_proxy = "9.81.1.160",
  oom_kill_disable = TRUE,
  os_type = "linux",
  oom_score_adj = NA_integer_,
  operating_system = "Boot2Docker",
  registry_config = registry_config,
  swap_limit = FALSE,
  system_time = "2015-03-10T11:11:23.730591467-07:00",
  server_version = "1.9.0"
  )
