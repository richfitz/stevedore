## version: 1.27
## method: get
## path: /containers/json
## code: 200
## response: [{"Id":"8dfafdbc3a40","Names":"/boring_feynman","Image":"ubuntu:latest","ImageID":"d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82","Command":"echo 1","Created":1367854155,"State":"Exited","Status":"Exit 0","Ports":[{"PrivatePort":2222,"PublicPort":3333,"Type":"tcp"}],"Labels":{"com.example.vendor":"Acme","com.example.license":"GPL","com.example.version":"1.0"},"SizeRw":12288,"SizeRootFs":0,"HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"2cdc4edb1ded3631c81f57966563e5c8525b81121bb3706a9a9a3ae102711f3f","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:02"}}},"Mounts":[{"Name":"fac362...80535","Source":"/data","Destination":"/data","Driver":"local","Mode":"ro,Z","RW":false,"Propagation":""}]},{"Id":"9cd87474be90","Names":"/coolName","Image":"ubuntu:latest","ImageID":"d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82","Command":"echo 222222","Created":1367854155,"State":"Exited","Status":"Exit 0","Ports":[],"Labels":{},"SizeRw":12288,"SizeRootFs":0,"HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"88eaed7b37b38c2a3f0c4bc796494fdf51b270c2d22656412a2ca5d559a64d7a","Gateway":"172.17.0.1","IPAddress":"172.17.0.8","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:08"}}},"Mounts":[]},{"Id":"3176a2479c92","Names":"/sleepy_dog","Image":"ubuntu:latest","ImageID":"d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82","Command":"echo 3333333333333333","Created":1367854154,"State":"Exited","Status":"Exit 0","Ports":[],"Labels":{},"SizeRw":12288,"SizeRootFs":0,"HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"8b27c041c30326d59cd6e6f510d4f8d1d570a228466f956edf7815508f78e30d","Gateway":"172.17.0.1","IPAddress":"172.17.0.6","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:06"}}},"Mounts":[]},{"Id":"4cb07b47f9fb","Names":"/running_cat","Image":"ubuntu:latest","ImageID":"d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82","Command":"echo 444444444444444444444444444444444","Created":1367854152,"State":"Exited","Status":"Exit 0","Ports":[],"Labels":{},"SizeRw":12288,"SizeRootFs":0,"HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"d91c7b2f0644403d7ef3095985ea0e2370325cd2332ff3a3225c4247328e66e9","Gateway":"172.17.0.1","IPAddress":"172.17.0.5","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:05"}}},"Mounts":[]}]
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

ports <- data_frame(i_p = NA_character_,
                    private_port = 2222L,
                    public_port = 3333L,
                    type = "tcp")
ports <- I(list(ports, ports[0, ], ports[0, ], ports[0, ]))

mounts <- data_frame(target = NA_character_,
                     source = "/data",
                     type = NA_character_,
                     read_only = NA,
                     bind_options = I(list(list(propagation = NA_character_))),
                     volume_options = I(list(list(
                       no_copy = NA, labels = NULL, driver_config = NULL))),
                     tmpfs_options = I(list(list(
                       size_bytes = NA_integer_, mode = NA_integer_))))
mounts <- I(list(mounts, mounts[0, ], mounts[0, ], mounts[0, ]))

nw1 <- list(
  i_pamconfig = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "2cdc4edb1ded3631c81f57966563e5c8525b81121bb3706a9a9a3ae102711f3f",
  gateway = "172.17.0.1",
  i_paddress = "172.17.0.2",
  i_pprefix_len = 16L,
  i_pv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:02")
nw2 <- list(
  i_pamconfig = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "88eaed7b37b38c2a3f0c4bc796494fdf51b270c2d22656412a2ca5d559a64d7a",
  gateway = "172.17.0.1",
  i_paddress = "172.17.0.8",
  i_pprefix_len = 16L,
  i_pv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:08")
nw3 <- list(
  i_pamconfig = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "8b27c041c30326d59cd6e6f510d4f8d1d570a228466f956edf7815508f78e30d",
  gateway = "172.17.0.1",
  i_paddress = "172.17.0.6",
  i_pprefix_len = 16L,
  i_pv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:06")
nw4 <- list(
  i_pamconfig = NULL,
  links = character(0),
  aliases = character(0),
  network_id = "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812",
  endpoint_id = "d91c7b2f0644403d7ef3095985ea0e2370325cd2332ff3a3225c4247328e66e9",
  gateway = "172.17.0.1",
  i_paddress = "172.17.0.5",
  i_pprefix_len = 16L,
  i_pv6_gateway = "",
  global_ipv6_address = "",
  global_ipv6_prefix_len = 0L,
  mac_address = "02:42:ac:11:00:05")

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
