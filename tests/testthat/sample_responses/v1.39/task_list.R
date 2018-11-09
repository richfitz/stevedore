## version: 1.39
## method: get
## path: /tasks
## code: 200
## response: [{"ID":"0kzzo1i0y4jz6027t0k7aezc7","Version":{"Index":71},"CreatedAt":"2016-06-07T21:07:31.171892745Z","UpdatedAt":"2016-06-07T21:07:31.376370513Z","Spec":{"ContainerSpec":{"Image":"redis"},"Resources":{"Limits":{},"Reservations":{}},"RestartPolicy":{"Condition":"any","MaxAttempts":0},"Placement":{}},"ServiceID":"9mnpnzenvg8p8tdbtq4wvbkcz","Slot":1,"NodeID":"60gvrl6tm78dmak4yl7srz94v","Status":{"Timestamp":"2016-06-07T21:07:31.290032978Z","State":"running","Message":"started","ContainerStatus":{"ContainerID":"e5d62702a1b48d01c3e02ca1e0212a250801fa8d67caca0b6f35919ebc12f035","PID":677}},"DesiredState":"running","NetworksAttachments":[{"Network":{"ID":"4qvuz4ko70xaltuqbt8956gd1","Version":{"Index":18},"CreatedAt":"2016-06-07T20:31:11.912919752Z","UpdatedAt":"2016-06-07T21:07:29.955277358Z","Spec":{"Name":"ingress","Labels":{"com.docker.swarm.internal":"true"},"DriverConfiguration":{},"IPAMOptions":{"Driver":{},"Configs":[{"Subnet":"10.255.0.0/16","Gateway":"10.255.0.1"}]}},"DriverState":{"Name":"overlay","Options":{"com.docker.network.driver.overlay.vxlanid_list":"256"}},"IPAMOptions":{"Driver":{"Name":"default"},"Configs":[{"Subnet":"10.255.0.0/16","Gateway":"10.255.0.1"}]}},"Addresses":"10.255.0.10/16"}]},{"ID":"1yljwbmlr8er2waf8orvqpwms","Version":{"Index":30},"CreatedAt":"2016-06-07T21:07:30.019104782Z","UpdatedAt":"2016-06-07T21:07:30.231958098Z","Name":"hopeful_cori","Spec":{"ContainerSpec":{"Image":"redis"},"Resources":{"Limits":{},"Reservations":{}},"RestartPolicy":{"Condition":"any","MaxAttempts":0},"Placement":{}},"ServiceID":"9mnpnzenvg8p8tdbtq4wvbkcz","Slot":1,"NodeID":"60gvrl6tm78dmak4yl7srz94v","Status":{"Timestamp":"2016-06-07T21:07:30.202183143Z","State":"shutdown","Message":"shutdown","ContainerStatus":{"ContainerID":"1cf8d63d18e79668b0004a4be4c6ee58cddfad2dae29506d8781581d0688a213"}},"DesiredState":"shutdown","NetworksAttachments":[{"Network":{"ID":"4qvuz4ko70xaltuqbt8956gd1","Version":{"Index":18},"CreatedAt":"2016-06-07T20:31:11.912919752Z","UpdatedAt":"2016-06-07T21:07:29.955277358Z","Spec":{"Name":"ingress","Labels":{"com.docker.swarm.internal":"true"},"DriverConfiguration":{},"IPAMOptions":{"Driver":{},"Configs":[{"Subnet":"10.255.0.0/16","Gateway":"10.255.0.1"}]}},"DriverState":{"Name":"overlay","Options":{"com.docker.network.driver.overlay.vxlanid_list":"256"}},"IPAMOptions":{"Driver":{"Name":"default"},"Configs":[{"Subnet":"10.255.0.0/16","Gateway":"10.255.0.1"}]}},"Addresses":"10.255.0.5/16"}]}]
NULL

container_spec <- list(
  image = "redis",
  labels = NULL,
  command = character(0),
  args = character(0),
  hostname = NA_character_,
  env = character(0),
  dir = NA_character_,
  user = NA_character_,
  groups = character(0),
  privileges = NULL,
  tty = NA,
  open_stdin = NA,
  read_only = NA,
  mounts = data_frame(
    target = character(0),
    source = character(0),
    type = character(0),
    read_only = logical(0),
    consistency = character(0),
    bind_options = I(list()),
    volume_options = I(list()),
    tmpfs_options = I(list())),
  stop_signal = NA_character_,
  stop_grace_period = NA_integer_,
  health_check = NULL,
  hosts = character(0),
  dns_config = NULL,
  secrets = data_frame(
    file = I(list()),
    secret_id = character(0),
    secret_name = character(0)),
  configs = data_frame(
    file = I(list()),
    config_id = character(0),
    config_name = character(0)),
  isolation = NA_character_,
  init = NA)

spec <- list(
  plugin_spec = NULL,
  container_spec = container_spec,
  network_attachment_spec = NULL,
  resources = list(
    limits = list(
      nano_cpus = NA_integer_,
      memory_bytes = NA_integer_,
      generic_resources = data_frame(
        named_resource_spec = I(list()),
        discrete_resource_spec = I(list()))),
    reservation = NULL),
  restart_policy = list(
    condition = "any",
    delay = NA_integer_,
    max_attempts = 0L,
    window = NA_integer_),
  placement = list(
    constraints = character(0),
    preferences = data_frame(
      spread = I(list())),
    platforms = data_frame(architecture = character(0),
                           os = character(0))),
  force_update = NA_integer_,
  runtime = NA_character_,
  networks = data_frame(
    target = character(),
    aliases = I(list())),
  log_driver = NULL)

status1 <- list(
  timestamp = "2016-06-07T21:07:31.290032978Z",
  state = "running",
  message = "started",
  err = NA_character_,
  container_status = list(
    container_id = "e5d62702a1b48d01c3e02ca1e0212a250801fa8d67caca0b6f35919ebc12f035",
    pid = 677L,
    exit_code = NA_integer_))

status2 <- list(
  timestamp = "2016-06-07T21:07:30.202183143Z",
  state = "shutdown",
  message = "shutdown",
  err = NA_character_,
  container_status = list(
    container_id = "1cf8d63d18e79668b0004a4be4c6ee58cddfad2dae29506d8781581d0688a213",
    pid = NA_integer_,
    exit_code = NA_integer_))

generic_resources <- data_frame(
  named_resource_spec = I(list()),
  discrete_resource_spec = I(list()))

data_frame(
  id = c("0kzzo1i0y4jz6027t0k7aezc7", "1yljwbmlr8er2waf8orvqpwms"),
  version = I(list(list(index = 71L), list(index = 30L))),
  created_at = c("2016-06-07T21:07:31.171892745Z",
                 "2016-06-07T21:07:30.019104782Z"),
  updated_at = c("2016-06-07T21:07:31.376370513Z",
                 "2016-06-07T21:07:30.231958098Z"),
  name = c(NA_character_, "hopeful_cori"),
  labels = I(list(character(0), character(0))),
  spec = I(list(spec, spec)),
  service_id = c("9mnpnzenvg8p8tdbtq4wvbkcz", "9mnpnzenvg8p8tdbtq4wvbkcz"),
  slot = c(1L, 1L),
  node_id = c("60gvrl6tm78dmak4yl7srz94v", "60gvrl6tm78dmak4yl7srz94v"),
  assigned_generic_resources = I(list(generic_resources, generic_resources)),
  status = I(list(status1, status2)),
  desired_state = c("running", "shutdown"))
