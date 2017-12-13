## version: 1.29
## method: get
## path: /system/df
## code: 200
## response: {"LayersSize":2561220560,"Images":[],"Containers":[{"Id":"0c729534847a8fe40082421924ccbbfb39ed12b7e685265c5cea9fce21e5341d","Names":["/nifty_knuth"],"Image":"hello-world","ImageID":"sha256:f2a91732366c0332ccd7afd2a5c4ff2b9af81f549370f7a19acd460f87686bc7","Command":"/hello","Created":1512634169,"Ports":[],"SizeRootFs":1848,"Labels":{},"State":"created","Status":"Created","HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"IPAMConfig":null,"Links":null,"Aliases":null,"NetworkID":"","EndpointID":"","Gateway":"","IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"","DriverOpts":null}}},"Mounts":[]}],"Volumes":[],"BuilderSize":0}
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
bridge <- list(
  ipam_config = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "",
  endpoint_id = "",
  gateway = "",
  ip_address = "",
  ip_prefix_len = 0L,
  ipv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "")
mounts <- data_frame(
  target = character(0),
  source = character(0),
  type = character(0),
  read_only = logical(0),
  consistency = character(0),
  bind_options = I(list()),
  volume_options = I(list()),
  tmpfs_options = I(list()))
ports <- data_frame(ip = character(0),
                    private_port = integer(0),
                    public_port = integer(0),
                    type = character(0))

containers <- data_frame(
  id = "0c729534847a8fe40082421924ccbbfb39ed12b7e685265c5cea9fce21e5341d",
  names = I(list("/nifty_knuth")),
  image = "hello-world",
  image_id =
    "sha256:f2a91732366c0332ccd7afd2a5c4ff2b9af81f549370f7a19acd460f87686bc7",
  command = "/hello",
  created = 1512634169L,
  ports = I(list(ports)),
  size_rw = NA_integer_,
  size_root_fs = 1848L,
  labels = I(list(character(0))),
  state = "created",
  status = "Created",
  host_config = I(list(list(network_mode = "default"))),
  network_settings = I(list(list(networks = list(bridge = bridge)))),
  mounts = I(list(mounts)))
rownames(containers) <- NULL

list(
  layers_size = 2561220560,
  images = data_frame(
    id = character(0),
    parent_id = character(0),
    repo_tags = I(list()),
    repo_digests = I(list()),
    created = integer(0),
    size = integer(0),
    shared_size = integer(0),
    virtual_size = integer(0),
    labels = I(list()),
    containers = integer(0)),
  containers = containers,
  volumes = data_frame(
    name = character(0),
    driver = character(0),
    mountpoint = character(0),
    status = I(list()),
    labels = I(list()),
    scope = character(0),
    options = I(list()),
    usage_data = I(list())))
