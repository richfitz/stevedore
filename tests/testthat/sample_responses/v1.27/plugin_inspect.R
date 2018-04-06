## version: 1.27
## method: get
## path: /plugins/{name}/json
## code: 200
## response: {"Id":"5724e2c8652da337ab2eedd19fc6fc0ec908e4bd907c7421bf6a8dfc70c4c078","Name":"tiborvass/sample-volume-plugin","Tag":"latest","Active":true,"Settings":{"Env":"DEBUG=0","Args":{},"Devices":null},"Config":{"Description":"A sample volume plugin for Docker","Documentation":"https://docs.docker.com/engine/extend/plugins/","Interface":{"Types":"docker.volumedriver/1.0","Socket":"plugins.sock"},"Entrypoint":["/usr/bin/sample-volume-plugin","/data"],"WorkDir":"","User":{},"Network":{"Type":""},"Linux":{"Capabilities":{},"AllowAllDevices":false,"Devices":null},"Mounts":null,"PropagatedMount":"/data","Env":[{"Name":"DEBUG","Description":"If set, prints debug messages","Settable":{},"Value":"0"}],"Args":{"Name":"args","Description":"command line arguments","Settable":{},"Value":[]}}}
NULL

## NOTE: compared with the version in the spec, I have set
## "Devices":null (both places it is used), and "Mounts":null - all
## were :{} which does not agree with the spec.  The example for 1.33
## shows something more sensible as output but that's using the new
## pattern.
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

settings <- list(
  mounts = data_frame(
    name = character(),
    description = character(),
    settable = I(list()),
    source = character(),
    destination = character(),
    type = character(),
    options = I(list())),
  env = "DEBUG=0",
  args = character(),
  devices = data_frame(
    name = character(),
    description = character(),
    settable = I(list()),
    path = character()))

config <- list(
  description = "A sample volume plugin for Docker",
  documentation = "https://docs.docker.com/engine/extend/plugins/",
  interface = list(
    types = data_frame(
      prefix = NA_character_,
      capability = NA_character_,
      version = NA_character_),
    socket = "plugins.sock"),
  entrypoint = c("/usr/bin/sample-volume-plugin", "/data"),
  work_dir = "",
  user = list(uid = NA_integer_, gid = NA_integer_),
  network = list(type = ""),
  linux = list(
    capabilities = character(0),
    allow_all_devices = FALSE,
    devices = data_frame(
      name = character(0),
      description = character(0),
      settable = I(list()),
      path = character(0))),
  propagated_mount = "/data",
  mounts = data_frame(
    name = character(0),
    description = character(0),
    settable = I(list()),
    source = character(0),
    destination = character(0),
    type = character(0),
    options = I(list())),
  env = data_frame(
    name = "DEBUG",
    description = "If set, prints debug messages",
    settable = I(list(character(0))),
    value = "0"),
  args = list(
    name = "args",
    description = "command line arguments",
    settable = character(0),
    value = character(0)),
  rootfs = NULL)

list(
  id = "5724e2c8652da337ab2eedd19fc6fc0ec908e4bd907c7421bf6a8dfc70c4c078",
  name = "tiborvass/sample-volume-plugin",
  enabled = NA,
  settings = settings,
  plugin_reference = NA_character_,
  config = config)
