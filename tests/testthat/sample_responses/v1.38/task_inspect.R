## version: 1.38
## method: get
## path: /tasks/{id}
## code: 200
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

## TODO: this could be done better:
assigned_generic_resources <- data_frame(
  named_resource_spec = I(list(
    list(kind = NA_character_, value = NA_character_),
    list(kind = "GPU", value = "UUID1"),
    list(kind = "GPU", value = "UUID2"))),
  discrete_resource_spec = I(list(
    list(kind = "SSD", value = 3L),
    list(kind = NA_character_, value = NA_integer_),
    list(kind = NA_character_, value = NA_integer_))))

list(
  id = "0kzzo1i0y4jz6027t0k7aezc7",
  version = list(index = 71L),
  created_at = "2016-06-07T21:07:31.171892745Z",
  updated_at = "2016-06-07T21:07:31.376370513Z",
  name = NA_character_,
  labels = NULL,
  spec = spec,
  service_id = "9mnpnzenvg8p8tdbtq4wvbkcz",
  slot = 1L,
  node_id = "60gvrl6tm78dmak4yl7srz94v",
  assigned_generic_resources = assigned_generic_resources,
  status = list(
    timestamp = "2016-06-07T21:07:31.290032978Z",
    state = "running",
    message = "started",
    err = NA_character_,
    container_status = list(
      container_id = "e5d62702a1b48d01c3e02ca1e0212a250801fa8d67caca0b6f35919ebc12f035",
      pid = 677L,
      exit_code = NA_integer_)),
  desired_state = "running")
