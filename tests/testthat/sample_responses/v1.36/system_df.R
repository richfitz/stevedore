## version: 1.36
## method: get
## path: /system/df
## code: 200
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}

images <- data_frame(
  id =
    "sha256:2b8fd9751c4c0f5dd266fcae00707e67a2545ef34f9a29354585f93dac906749",
  parent_id = "",
  repo_tags = I(list("busybox:latest")),
  repo_digests = I(list("busybox@sha256:a59906e33509d14c036c8678d687bd4eec81ed7c4b8ce907b888c607f6a1e0e6")),
  created = 1466724217,
  size = 1092588,
  shared_size = 0,
  virtual_size = 1092588,
  labels = I(list(character())),
  containers = 1)

containers <- data_frame(
    id = "e575172ed11dc01bfce087fb27bee502db149e1a0fad7c296ad300bbff178148",
    names = I(list("/top")),
    image = "busybox",
    image_id =
      "sha256:2b8fd9751c4c0f5dd266fcae00707e67a2545ef34f9a29354585f93dac906749",
    command = "top",
    created = 1472592424L,
    ports = I(list(data_frame(
      ip = character(),
      private_port = integer(),
      public_port = integer(),
      type = character()))),
    size_rw = NA_integer_,
    size_root_fs = 1092588L,
    labels = I(list(character())),
    state = "exited",
    status = "Exited (0) 56 minutes ago",
    host_config = I(list(list(network_mode = "default"))),
    network_settings = I(list(list(
      networks = list(
        bridge = list(
          ipam_config = NULL,
          links = character(),
          aliases = character(),
          network_id =
            "d687bc59335f0e5c9ee8193e5612e8aee000c8c62ea170cfb99c098f95899d92",
          endpoint_id =
            "8ed5115aeaad9abb174f68dcf135b49f11daf597678315231a32ca28441dec6a",
          gateway = "172.18.0.1",
          ip_address = "172.18.0.2",
          ip_prefix_len = 16L,
          ipv6_gateway = "",
          global_ipv6_address = "",
          global_ipv6_prefix_len = 0L,
          mac_address = "02:42:ac:12:00:02",
          driver_opts = NULL))))),
    mounts = I(list(data_frame(
      target = character(),
      source = character(),
      type = character(),
      read_only = logical(),
      consistency = character(),
      bind_options = I(list()),
      volume_options = I(list()),
      tmpfs_options = I(list())))))

volumes <- data_frame(
  name = "my-volume",
  driver = "local",
  mountpoint = "/var/lib/docker/volumes/my-volume/_data",
  created_at = NA_character_,
  status = I(list(list())),
  labels = I(list(character())),
  scope = "local",
  options = I(list(character())),
  usage_data = I(list(list(size = 10920104, ref_count = 2))))

list(
  layers_size = 1092588,
  images = images,
  containers = containers,
  volumes = volumes)
