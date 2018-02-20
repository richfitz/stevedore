## version: 1.36
## method: get
## path: /containers/json
## code: 200
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

ports <- data_frame(ip = NA_character_,
                    private_port = 2222L,
                    public_port = 3333L,
                    type = "tcp")
ports <- I(list(ports, ports[0, ], ports[0, ], ports[0, ]))

mounts <- data_frame(target = NA_character_,
                     source = "/data",
                     type = NA_character_,
                     read_only = NA,
                     consistency = NA_character_,
                     bind_options = I(list(list(propagation = NA_character_))),
                     volume_options = I(list(list(
                       no_copy = NA, labels = NULL, driver_config = NULL))),
                     tmpfs_options = I(list(list(
                       size_bytes = NA_integer_, mode = NA_integer_))))
mounts <- I(list(mounts, mounts[0, ], mounts[0, ], mounts[0, ]))

nw1 <- list(
  ipam_config = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "2cdc4edb1ded3631c81f57966563e5c8525b81121bb3706a9a9a3ae102711f3f",
  gateway = "172.17.0.1",
  ip_address = "172.17.0.2",
  ip_prefix_len = 16L,
  ipv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:02",
  driver_opts = NULL)
nw2 <- list(
  ipam_config = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "88eaed7b37b38c2a3f0c4bc796494fdf51b270c2d22656412a2ca5d559a64d7a",
  gateway = "172.17.0.1",
  ip_address = "172.17.0.8",
  ip_prefix_len = 16L,
  ipv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:08",
  driver_opts = NULL)
nw3 <- list(
  ipam_config = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "8b27c041c30326d59cd6e6f510d4f8d1d570a228466f956edf7815508f78e30d",
  gateway = "172.17.0.1",
  ip_address = "172.17.0.6",
  ip_prefix_len = 16L,
  ipv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:06",
  driver_opts = NULL)
nw4 <- list(
  ipam_config = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "d91c7b2f0644403d7ef3095985ea0e2370325cd2332ff3a3225c4247328e66e9",
  gateway = "172.17.0.1",
  ip_address = "172.17.0.5",
  ip_prefix_len = 16L,
  ipv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:05",
  driver_opts = NULL)

network_settings <- I(list(list(networks = list(bridge = nw1)),
                           list(networks = list(bridge = nw2)),
                           list(networks = list(bridge = nw3)),
                           list(networks = list(bridge = nw4))))

data_frame(
  id = c("8dfafdbc3a40", "9cd87474be90", "3176a2479c92", "4cb07b47f9fb"),
  names = I(list(
    "/boring_feynman", "/coolName", "/sleepy_dog",  "/running_cat")),
  image = "ubuntu:latest",
  image_id = "d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82",
  command = c("echo 1", "echo 222222", "echo 3333333333333333",
              "echo 444444444444444444444444444444444"),
  created = c(1367854155L, 1367854155L, 1367854154L, 1367854152L),
  ports = ports,
  size_rw = 12288L,
  size_root_fs = 0L,
  labels = I(list(c("com.example.vendor" = "Acme",
                    "com.example.license" = "GPL",
                    "com.example.version" = "1.0"),
                  character(), character(), character())),
  state = "Exited",
  status = "Exit 0",
  host_config = I(rep(list(list(network_mode = "default")), 4)),
  network_settings = network_settings,
  mounts = mounts)
