## version: 1.29
## method: get
## path: /containers/{id}/json
## code: 200
## response: {"Id":"3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c","Created":"2017-11-28T16:49:59.3872013Z","Path":"sleep","Args":["10000"],"State":{"Status":"running","Running":true,"Paused":false,"Restarting":false,"OOMKilled":false,"Dead":false,"Pid":10036,"ExitCode":0,"Error":"","StartedAt":"2017-11-28T16:51:47.2856007Z","FinishedAt":"0001-01-01T00:00:00Z"},"Image":"sha256:053cde6e8953ebd834df8f6382e68be83adb39bfc063e40b0fc61b4b333938f1","ResolvConfPath":"/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/resolv.conf","HostnamePath":"/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/hostname","HostsPath":"/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/hosts","LogPath":"/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c-json.log","Name":"/testp1","RestartCount":0,"Driver":"overlay2","Platform":"linux","MountLabel":"","ProcessLabel":"","AppArmorProfile":"","ExecIDs":null,"HostConfig":{"Binds":null,"ContainerIDFile":"","LogConfig":{"Type":"json-file","Config":{}},"NetworkMode":"default","PortBindings":{"2345/tcp":[{"HostIp":"","HostPort":"1234"}]},"RestartPolicy":{"Name":"no","MaximumRetryCount":0},"AutoRemove":false,"VolumeDriver":"","VolumesFrom":null,"CapAdd":null,"CapDrop":null,"Dns":[],"DnsOptions":[],"DnsSearch":[],"ExtraHosts":null,"GroupAdd":null,"IpcMode":"shareable","Cgroup":"","Links":null,"OomScoreAdj":0,"PidMode":"","Privileged":false,"PublishAllPorts":false,"ReadonlyRootfs":false,"SecurityOpt":null,"UTSMode":"","UsernsMode":"","ShmSize":67108864,"Runtime":"runc","ConsoleSize":[0,0],"Isolation":"","CpuShares":0,"Memory":0,"NanoCpus":0,"CgroupParent":"","BlkioWeight":0,"BlkioWeightDevice":[],"BlkioDeviceReadBps":null,"BlkioDeviceWriteBps":null,"BlkioDeviceReadIOps":null,"BlkioDeviceWriteIOps":null,"CpuPeriod":0,"CpuQuota":0,"CpuRealtimePeriod":0,"CpuRealtimeRuntime":0,"CpusetCpus":"","CpusetMems":"","Devices":[],"DeviceCgroupRules":null,"DiskQuota":0,"KernelMemory":0,"MemoryReservation":0,"MemorySwap":0,"MemorySwappiness":null,"OomKillDisable":false,"PidsLimit":0,"Ulimits":null,"CpuCount":0,"CpuPercent":0,"IOMaximumIOps":0,"IOMaximumBandwidth":0},"GraphDriver":{"Data":{"LowerDir":"/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78-init/diff:/var/lib/docker/overlay2/361379d8c431e5b334d3227519bec4f35e2b0dcd26484f3935e8fc72fa2d7254/diff","MergedDir":"/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/merged","UpperDir":"/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/diff","WorkDir":"/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/work"},"Name":"overlay2"},"Mounts":[],"Config":{"Hostname":"3949fff2e8dd","Domainname":"","User":"","AttachStdin":false,"AttachStdout":true,"AttachStderr":true,"ExposedPorts":{"2345/tcp":{}},"Tty":false,"OpenStdin":false,"StdinOnce":false,"Env":["PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"],"Cmd":["sleep","10000"],"Image":"alpine:latest","Volumes":null,"WorkingDir":"","Entrypoint":null,"OnBuild":null,"Labels":{}},"NetworkSettings":{"Bridge":"","SandboxID":"ce25166af4f3d2a056e3b0fc460a024bc2f97391bb522188178ed9af0b20cafa","HairpinMode":false,"LinkLocalIPv6Address":"","LinkLocalIPv6PrefixLen":0,"Ports":{"2345/tcp":[{"HostIp":"0.0.0.0","HostPort":"1234"}]},"SandboxKey":"/var/run/docker/netns/ce25166af4f3","SecondaryIPAddresses":null,"SecondaryIPv6Addresses":null,"EndpointID":"d360ac009927ca15066ea4f8e59066aa27374417b977c4edae607dd06af5e66f","Gateway":"172.17.0.1","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","MacAddress":"02:42:ac:11:00:02","Networks":{"bridge":{"IPAMConfig":null,"Links":null,"Aliases":null,"NetworkID":"1999b72a62a541c04206ea19cd3426fadd62bce8551898e8daedeee2b6428ff0","EndpointID":"d360ac009927ca15066ea4f8e59066aa27374417b977c4edae607dd06af5e66f","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:02","DriverOpts":null}}}}
NULL

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
host_config <- list(
  cpu_shares = 0L,
  memory = 0L,
  cgroup_parent = "",
  blkio_weight = 0L,
  blkio_weight_device = data_frame(path = character(0), weight = integer(0)),
  blkio_device_read_bps = data_frame(path = character(0), rate = integer(0)),
  blkio_device_write_bps = data_frame(path = character(0), rate = integer(0)),
  blkio_device_read_iops = data_frame(path = character(0), rate = integer(0)),
  blkio_device_write_iops =
    data_frame(path = character(0), rate = integer(0)),
  cpu_period = 0L,
  cpu_quota = 0L,
  cpu_realtime_period = 0L,
  cpu_realtime_runtime = 0L,
  cpuset_cpus = "",
  cpuset_mems = "",
  devices = data_frame(path_on_host = character(0),
                       path_in_container = character(0),
                       cgroup_permissions = character(0)),
  device_cgroup_rules = character(0),
  disk_quota = 0L,
  kernel_memory = 0L,
  memory_reservation = 0L,
  memory_swap = 0L,
  memory_swappiness = NA_integer_,
  nano_cpus = NA_integer_,
  oom_kill_disable = FALSE,
  pids_limit = 0L,
  ulimits = data_frame(name = character(), soft = integer(), hard = integer()),
  cpu_count = 0L,
  cpu_percent = 0L,
  io_maximum_iops = 0L,
  io_maximum_bandwidth = 0L,
  binds = character(0),
  container_idfile = "",
  log_config = list(type = "json-file", config = character()),
  network_mode = "default",
  ## TODO: This is not yet correct, but I think that's a separate
  ## problem to the one that sparked this.  I suspect that the docker
  ## schema started diverging around 1.29 on the treatment here but I
  ## don't see any mention in the spec.
  port_bindings = list("2345/tcp" =
                         list(host_ip = NA_character_,
                              host_port = NA_character_)),
  restart_policy = list(name = "no", maximum_retry_count = 0L),
  auto_remove = FALSE,
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
  ipc_mode = "shareable",
  cgroup = "",
  links = character(0),
  oom_score_adj = 0L,
  pid_mode = "",
  privileged = FALSE,
  publish_all_ports = FALSE,
  readonly_rootfs = FALSE,
  security_opt = character(0),
  storage_opt = NULL,
  tmpfs = NULL,
  uts_mode = "",
  userns_mode = "",
  shm_size = 67108864L,
  sysctls = NULL,
  runtime = "runc",
  console_size = c(0L, 0L),
  isolation = "")

graph_driver <- list(
  name = "overlay2",
  data = c(
    lower_dir = "/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78-init/diff:/var/lib/docker/overlay2/361379d8c431e5b334d3227519bec4f35e2b0dcd26484f3935e8fc72fa2d7254/diff",
    merged_dir = "/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/merged",
    upper_dir = "/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/diff",
    work_dir = "/var/lib/docker/overlay2/f1a8016eaa88290c737d3a61bfac8bdb7e818c32e09dfd98aa9e3223c89d5b78/work"))

mounts <- data.frame(
  type = character(),
  name = character(),
  source = character(),
  destination = character(),
  driver = character(),
  mode = character(),
  rw = logical(),
  propagation = character(),
  stringsAsFactors = FALSE)

config <- list(
  hostname = "3949fff2e8dd",
  domainname = "",
  user = "",
  attach_stdin = FALSE,
  attach_stdout = TRUE,
  attach_stderr = TRUE,
  exposed_ports = list("2345/tcp" = structure(list(), names = character(0))),
  tty = FALSE,
  open_stdin = FALSE,
  stdin_once = FALSE,
  env = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  cmd = c("sleep", "10000"),
  healthcheck = NULL,
  args_escaped = NA,
  image = "alpine:latest",
  volumes = NULL,
  working_dir = "",
  entrypoint = character(0),
  network_disabled = NA,
  mac_address = NA_character_,
  on_build = character(0),
  labels = character(0),
  stop_signal = NA_character_,
  stop_timeout = NA_integer_,
  shell = character(0)
)

network_settings <- list(
  bridge = "",
  gateway = "172.17.0.1",
  address = NA_character_,
  ip_prefix_len = 16L,
  mac_address = "02:42:ac:11:00:02",
  port_mapping = NA_character_,
  ports = list("2345/tcp" = data.frame(
                 host_ip = "0.0.0.0",
                 host_port = "1234",
                 stringsAsFactors = FALSE)))

list(
  id = "3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c",
  created = "2017-11-28T16:49:59.3872013Z",
  path = "sleep",
  args = "10000",
  state = list(
    status = "running",
    running = TRUE,
    paused = FALSE,
    restarting = FALSE,
    oom_killed = FALSE,
    dead = FALSE,
    pid = 10036L,
    exit_code = 0L,
    error = "",
    started_at = "2017-11-28T16:51:47.2856007Z",
    finished_at = "0001-01-01T00:00:00Z"),
  image = "sha256:053cde6e8953ebd834df8f6382e68be83adb39bfc063e40b0fc61b4b333938f1",
  resolv_conf_path = "/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/resolv.conf",
  hostname_path = "/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/hostname",
  hosts_path = "/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/hosts",
  log_path = "/var/lib/docker/containers/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c/3949fff2e8dd8d0df1a7419b3f1f99cb0530558df91f627bf8d50e18e9cb5e6c-json.log",
  node = NULL,
  name = "/testp1",
  restart_count = 0L,
  driver = "overlay2",
  mount_label = "",
  process_label = "",
  app_armor_profile = "",
  exec_ids = NA_character_,
  host_config = host_config,
  graph_driver = graph_driver,
  size_rw = NA_integer_,
  size_root_fs = NA_integer_,
  mounts = mounts,
  config = config,
  network_settings = network_settings)
