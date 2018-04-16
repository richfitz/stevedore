## version: 1.36
## method: get
## path: /plugins/{name}/json
## code: 200
NULL

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

settings <- list(
  mounts = data_frame(
    name = "some-mount",
    description = "This is a mount that's used by the plugin.",
    settable = structure(list("string"), class = "AsIs"),
    source = "/var/lib/docker/plugins/",
    destination = "/mnt/state",
    type = "bind",
    options = I(list(c("rbind", "rw")))),
  env = "DEBUG=0",
  args = "string",
  devices = data_frame(
    name = "string",
    description = "string",
    settable = I(list("string")),
    path = "/dev/fuse"))

config <- list(
  docker_version = "17.06.0-ce",
  description = "A sample volume plugin for Docker",
  documentation = "https://docs.docker.com/engine/extend/plugins/",
  interface = list(
    types = data_frame(
      prefix = NA_character_,
      capability = NA_character_,
      version = NA_character_),
    socket = "plugins.sock"),
  entrypoint = c("/usr/bin/sample-volume-plugin", "/data"),
  work_dir = "/bin/",
  user = list(uid = 1000L, gid = 1000L),
  network = list(type = "host"),
  linux = list(
    capabilities = c("CAP_SYS_ADMIN", "CAP_SYSLOG"),
    allow_all_devices = FALSE,
    devices = data_frame(
      name = "string",
      description = "string",
      settable = I(list("string")),
      path = "/dev/fuse")),
  propagated_mount = "/mnt/volumes",
  ipc_host = FALSE,
  pid_host = FALSE,
  mounts = data_frame(
    name = "some-mount",
    description = "This is a mount that's used by the plugin.",
    settable = structure(list("string"), class = "AsIs"),
    source = "/var/lib/docker/plugins/",
    destination = "/mnt/state",
    type = "bind",
    options = I(list(c("rbind", "rw")))),
  env = data_frame(
    name = "DEBUG",
    description = "If set, prints debug messages",
    settable = I(list(character(0))),
    value = "0"),
  args = list(
    name = "args",
    description = "command line arguments",
    settable = "string",
    value = "string"),
  rootfs = list(
    type = "layers",
    diff_ids = c(
      "sha256:675532206fbf3030b8458f88d6e26d4eb1577688a25efec97154c94e8b6b4887",
      "sha256:e216a057b1cb1efc11f8a268f37ef62083e70b1b38323ba252e25ac88904a7e8"
    )))


list(
  id = "5724e2c8652da337ab2eedd19fc6fc0ec908e4bd907c7421bf6a8dfc70c4c078",
  name = "tiborvass/sample-volume-plugin",
  enabled = TRUE,
  settings = settings,
  plugin_reference = "localhost:5000/tiborvass/sample-volume-plugin:latest",
  config = config)
