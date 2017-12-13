## version: 1.31
## method: get
## path: /containers/{id}/json
## code: 200
## response: {"AppArmorProfile":"","Args":["-c","exit 9"],"Config":{"AttachStderr":true,"AttachStdin":false,"AttachStdout":true,"Cmd":["/bin/sh","-c","exit 9"],"Domainname":"","Env":"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin","Hostname":"ba033ac44011","Image":"ubuntu","Labels":{"com.example.vendor":"Acme","com.example.license":"GPL","com.example.version":"1.0"},"MacAddress":"","NetworkDisabled":false,"OpenStdin":false,"StdinOnce":false,"Tty":false,"User":"","Volumes":{"/volumes/data":{}},"WorkingDir":"","StopSignal":"SIGTERM","StopTimeout":10},"Created":"2015-01-06T15:47:31.485331387Z","Driver":"devicemapper","HostConfig":{"MaximumIOps":0,"MaximumIOBps":0,"BlkioWeight":0,"BlkioWeightDevice":[{}],"BlkioDeviceReadBps":[{}],"BlkioDeviceWriteBps":[{}],"BlkioDeviceReadIOps":[{}],"BlkioDeviceWriteIOps":[{}],"ContainerIDFile":"","CpusetCpus":"","CpusetMems":"","CpuPercent":80,"CpuShares":0,"CpuPeriod":100000,"CpuRealtimePeriod":1000000,"CpuRealtimeRuntime":10000,"Devices":[],"IpcMode":"","LxcConf":[],"Memory":0,"MemorySwap":0,"MemoryReservation":0,"KernelMemory":0,"OomKillDisable":false,"OomScoreAdj":500,"NetworkMode":"bridge","PidMode":"","PortBindings":{},"Privileged":false,"ReadonlyRootfs":false,"PublishAllPorts":false,"RestartPolicy":{"MaximumRetryCount":2,"Name":"on-failure"},"LogConfig":{"Type":"json-file"},"Sysctls":{"net.ipv4.ip_forward":"1"},"Ulimits":[{}],"VolumeDriver":"","ShmSize":67108864},"HostnamePath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hostname","HostsPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hosts","LogPath":"/var/lib/docker/containers/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b-json.log","Id":"ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39","Image":"04c5d3b7b0656168630d3ba35d8889bd0e9caafcaeb3004d2bfbc47e7c5d35d2","MountLabel":"","Name":"/boring_euclid","NetworkSettings":{"Bridge":"","SandboxID":"","HairpinMode":false,"LinkLocalIPv6Address":"","LinkLocalIPv6PrefixLen":0,"SandboxKey":"","SecondaryIPAddresses":{},"SecondaryIPv6Addresses":{},"EndpointID":"","Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","MacAddress":"","Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"7587b82f0dada3656fda26588aee72630c6fab1536d36e394b2bfbcf898c971d","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:12:00:02"}}},"Path":"/bin/sh","ProcessLabel":"","ResolvConfPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/resolv.conf","RestartCount":1,"State":{"Error":"","ExitCode":9,"FinishedAt":"2015-01-06T15:47:32.080254511Z","OOMKilled":false,"Dead":false,"Paused":false,"Pid":0,"Restarting":false,"Running":true,"StartedAt":"2015-01-06T15:47:32.072697474Z","Status":"running"},"Mounts":[{"Name":"fac362...80535","Source":"/data","Destination":"/data","Driver":"local","Mode":"ro,Z","RW":false,"Propagation":""}]}
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

blkio_obj = data_frame(
  path = NA_character_,
  rate = NA_integer_)

host_config <- list(
  cpu_shares = 0L,
  memory = 0L,
  cgroup_parent = NA_character_,
  blkio_weight = 0L,
  blkio_weight_device = data_frame(
    path = NA_character_,
    weight = NA_integer_),
  blkio_device_read_bps = blkio_obj,
  blkio_device_write_bps = blkio_obj,
  blkio_device_read_iops = blkio_obj,
  blkio_device_write_iops = blkio_obj,
  cpu_period = 100000L,
  cpu_quota = NA_integer_,
  cpu_realtime_period = 1000000L,
  cpu_realtime_runtime = 10000L,
  cpuset_cpus = "",
  cpuset_mems = "",
  devices = data_frame(
    path_on_host = character(),
    path_in_container = character(),
    cgroup_permissions = character()),
  device_cgroup_rules = character(0),
  disk_quota = NA_integer_,
  kernel_memory = 0L,
  memory_reservation = 0L,
  memory_swap = 0L,
  memory_swappiness = NA_integer_,
  nano_cpus = NA_integer_,
  oom_kill_disable = FALSE,
  pids_limit = NA_integer_,
  ulimits = data_frame(
    name = NA_character_,
    soft = NA_integer_,
    hard = NA_integer_),
  cpu_count = NA_integer_,
  cpu_percent = 80L,
  io_maximum_iops = NA_integer_,
  io_maximum_bandwidth = NA_integer_,
  binds = character(0),
  container_idfile = "",
  log_config = list(
    type = "json-file",
    config = NULL),
  network_mode = "bridge",
  port_bindings = list(),
  restart_policy = list(
    name = "on-failure",
    maximum_retry_count = 2L),
  auto_remove = NA,
  volume_driver = "",
  volumes_from = character(0),
  mounts = data_frame(
    target = character(),
    source = character(),
    type = character(),
    read_only = logical(),
    consistency = character(),
    bind_options = I(list()),
    volume_options = I(list()),
    tmpfs_options = I(list())),
  cap_add = character(0),
  cap_drop = character(0),
  dns = character(0),
  dns_options = character(0),
  dns_search = character(0),
  extra_hosts = character(0),
  group_add = character(0),
  ipc_mode = "",
  cgroup = NA_character_,
  links = character(0),
  oom_score_adj = 500L,
  pid_mode = "",
  privileged = FALSE,
  publish_all_ports = FALSE,
  readonly_rootfs = FALSE,
  security_opt = character(0),
  storage_opt = NULL,
  tmpfs = NULL,
  uts_mode = NA_character_,
  userns_mode = NA_character_,
  shm_size = 67108864L,
  sysctls = c("net.ipv4.ip_forward" = "1"),
  runtime = NA_character_,
  console_size = integer(0),
  isolation = NA_character_)

config <- list(
  hostname = "ba033ac44011",
  domainname = "",
  user = "",
  attach_stdin = FALSE,
  attach_stdout = TRUE,
  attach_stderr = TRUE,
  exposed_ports = NULL,
  tty = FALSE,
  open_stdin = FALSE,
  stdin_once = FALSE,
  env = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  cmd = c("/bin/sh", "-c", "exit 9"),
  healthcheck = NULL,
  args_escaped = NA,
  image = "ubuntu",
  ## TODO: this looks incorrect
  volumes = list(additional_properties = NULL),
  working_dir = "",
  entrypoint = character(0),
  network_disabled = FALSE,
  mac_address = "",
  on_build = character(0),
  labels = c(
    com.example.vendor = "Acme",
    com.example.license = "GPL",
    com.example.version = "1.0"),
  stop_signal = "SIGTERM",
  stop_timeout = 10L,
  shell = character(0))

## TODO: this is totally wrong - I see quite a bit of network settings
## in the underlying object, but this is filling in an empty one.
## Worse than that, it's missing some of the required fields - there
## should be a "networks" field here.
network_settings <- list(
  bridge = "",
  gateway = "",
  address = NA_character_,
  ip_prefix_len = 0L,
  mac_address = "",
  port_mapping = NA_character_,
  ports = NULL)

list(
  id = "ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39",
  created = "2015-01-06T15:47:31.485331387Z",
  path = "/bin/sh",
  args = c("-c", "exit 9"),
  state = list(
    status = "running",
    running = TRUE,
    paused = FALSE,
    restarting = FALSE,
    oom_killed = FALSE,
    dead = FALSE,
    pid = 0L,
    exit_code = 9L,
    error = "",
    started_at = "2015-01-06T15:47:32.072697474Z",
    finished_at = "2015-01-06T15:47:32.080254511Z"
  ),
  image = "04c5d3b7b0656168630d3ba35d8889bd0e9caafcaeb3004d2bfbc47e7c5d35d2",
  resolv_conf_path = "/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/resolv.conf",
  hostname_path = "/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hostname",
  hosts_path = "/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hosts",
  log_path = "/var/lib/docker/containers/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b-json.log",
  node = NULL,
  name = "/boring_euclid",
  restart_count = 1L,
  driver = "devicemapper",
  mount_label = "",
  process_label = "",
  app_armor_profile = "",
  exec_ids = NA_character_,
  host_config = host_config,
  graph_driver = NULL,
  size_rw = NA_integer_,
  size_root_fs = NA_integer_,
  mounts = data.frame(
    type = NA_character_,
    name = "fac362...80535",
    source = "/data",
    destination = "/data",
    driver = "local",
    mode = "ro,Z",
    rw = FALSE,
    propagation = "",
    stringsAsFactors = FALSE),
  config = config,
  network_settings = network_settings)
