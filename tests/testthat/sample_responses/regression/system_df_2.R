## version: 1.29
## method: get
## path: /system/df
## code: 200
## response: {"LayersSize":2561220560,"Images":[],"Containers":[{"Id":"3ea0307e0e8cd40a6c053b50fac8b6ee3f1ee6e24ea6aecfd3bfc60375848bf0","Names":["/dreamy_austin"],"Image":"richfitz/iterate","ImageID":"sha256:90ca6c2e0bd070af32c0f4ef0f9039b9dc760db29ace79a56162d5e620283614","Command":"/usr/local/bin/iterate /bin/sh -c 'chmod +x /usr/local/bin/iterate'","Created":1512634681,"Ports":[],"SizeRootFs":3966223,"Labels":{},"State":"created","Status":"Created","HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"IPAMConfig":null,"Links":null,"Aliases":null,"NetworkID":"","EndpointID":"","Gateway":"","IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"","DriverOpts":null}}},"Mounts":[]},{"Id":"0c729534847a8fe40082421924ccbbfb39ed12b7e685265c5cea9fce21e5341d","Names":["/nifty_knuth"],"Image":"hello-world","ImageID":"sha256:f2a91732366c0332ccd7afd2a5c4ff2b9af81f549370f7a19acd460f87686bc7","Command":"/hello","Created":1512634169,"Ports":[],"SizeRootFs":1848,"Labels":{},"State":"created","Status":"Created","HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"IPAMConfig":null,"Links":null,"Aliases":null,"NetworkID":"","EndpointID":"","Gateway":"","IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"","DriverOpts":null}}},"Mounts":[]}],"Volumes":[],"BuilderSize":0}
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
bridge <- list(
  i_pamconfig = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "",
  endpoint_id = "",
  gateway = "",
  i_paddress = "",
  i_pprefix_len = 0L,
  i_pv6_gateway = "",
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
ports <- data_frame(i_p = character(0),
                    private_port = integer(0),
                    public_port = integer(0),
                    type = character(0))

containers <- data_frame(
  id = c("3ea0307e0e8cd40a6c053b50fac8b6ee3f1ee6e24ea6aecfd3bfc60375848bf0",
         "0c729534847a8fe40082421924ccbbfb39ed12b7e685265c5cea9fce21e5341d"),
  names = I(list("/dreamy_austin", "/nifty_knuth")),
  image = c("richfitz/iterate", "hello-world"),
  image_id = c(
    "sha256:90ca6c2e0bd070af32c0f4ef0f9039b9dc760db29ace79a56162d5e620283614",
    "sha256:f2a91732366c0332ccd7afd2a5c4ff2b9af81f549370f7a19acd460f87686bc7"),
  command = c(
    "/usr/local/bin/iterate /bin/sh -c 'chmod +x /usr/local/bin/iterate'",
    "/hello"),
  created = c(1512634681L, 1512634169L),
  ports = I(list(ports, ports)),
  size_rw = NA_integer_,
  size_root_fs = c(3966223L, 1848L),
  labels = I(list(character(0), character(0))),
  state = "created",
  status = "Created",
  host_config = I(list(list(network_mode = "default"),
                       list(network_mode = "default"))),
  network_settings = I(list(list(networks = list(bridge = bridge)),
                            list(networks = list(bridge = bridge)))),
  mounts = I(list(mounts, mounts)))
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
