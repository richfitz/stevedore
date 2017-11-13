## version: v1.29
## method: get
## path: /containers/{id}/json
## code: 200
## response: {"AppArmorProfile":"","Args":["-c","exit 9"],"Config":{"AttachStderr":true,"AttachStdin":false,"AttachStdout":true,"Cmd":["/bin/sh","-c","exit 9"],"Domainname":"","Env":"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin","Hostname":"ba033ac44011","Image":"ubuntu","Labels":{"com.example.vendor":"Acme","com.example.license":"GPL","com.example.version":"1.0"},"MacAddress":"","NetworkDisabled":false,"OpenStdin":false,"StdinOnce":false,"Tty":false,"User":"","Volumes":{"/volumes/data":{}},"WorkingDir":"","StopSignal":"SIGTERM","StopTimeout":10},"Created":"2015-01-06T15:47:31.485331387Z","Driver":"devicemapper","HostConfig":{"MaximumIOps":0,"MaximumIOBps":0,"BlkioWeight":0,"BlkioWeightDevice":[{}],"BlkioDeviceReadBps":[{}],"BlkioDeviceWriteBps":[{}],"BlkioDeviceReadIOps":[{}],"BlkioDeviceWriteIOps":[{}],"ContainerIDFile":"","CpusetCpus":"","CpusetMems":"","CpuPercent":80,"CpuShares":0,"CpuPeriod":100000,"CpuRealtimePeriod":1000000,"CpuRealtimeRuntime":10000,"Devices":[],"IpcMode":"","LxcConf":[],"Memory":0,"MemorySwap":0,"MemoryReservation":0,"KernelMemory":0,"OomKillDisable":false,"OomScoreAdj":500,"NetworkMode":"bridge","PidMode":"","PortBindings":{},"Privileged":false,"ReadonlyRootfs":false,"PublishAllPorts":false,"RestartPolicy":{"MaximumRetryCount":2,"Name":"on-failure"},"LogConfig":{"Type":"json-file"},"Sysctls":{"net.ipv4.ip_forward":"1"},"Ulimits":[{}],"VolumeDriver":"","ShmSize":67108864},"HostnamePath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hostname","HostsPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hosts","LogPath":"/var/lib/docker/containers/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b-json.log","Id":"ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39","Image":"04c5d3b7b0656168630d3ba35d8889bd0e9caafcaeb3004d2bfbc47e7c5d35d2","MountLabel":"","Name":"/boring_euclid","NetworkSettings":{"Bridge":"","SandboxID":"","HairpinMode":false,"LinkLocalIPv6Address":"","LinkLocalIPv6PrefixLen":0,"SandboxKey":"","SecondaryIPAddresses":{},"SecondaryIPv6Addresses":{},"EndpointID":"","Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","MacAddress":"","Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"7587b82f0dada3656fda26588aee72630c6fab1536d36e394b2bfbcf898c971d","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:12:00:02"}}},"Path":"/bin/sh","ProcessLabel":"","ResolvConfPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/resolv.conf","RestartCount":1,"State":{"Error":"","ExitCode":9,"FinishedAt":"2015-01-06T15:47:32.080254511Z","OOMKilled":false,"Dead":false,"Paused":false,"Pid":0,"Restarting":false,"Running":true,"StartedAt":"2015-01-06T15:47:32.072697474Z","Status":"running"},"Mounts":[{"Name":"fac362...80535","Source":"/data","Destination":"/data","Driver":"local","Mode":"ro,Z","RW":false,"Propagation":""}]}
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
    o_omkilled = FALSE,
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
  host_config = list(),
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
    r_w = FALSE,
    propagation = "",
    stringsAsFactors = FALSE),
  config = list(),
  network_settings = list()
)
