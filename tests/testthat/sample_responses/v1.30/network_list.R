## version: 1.30
## method: get
## path: /networks
## code: 200
## response: [{"Name":"bridge","Id":"f2de39df4171b0dc801e8002d1d999b77256983dfc63041c0f34030aa3977566","Created":"2016-10-19T06:21:00.416543526Z","Scope":"local","Driver":"bridge","EnableIPv6":false,"Internal":false,"Attachable":false,"Ingress":false,"IPAM":{"Driver":"default","Config":[{"Subnet":"172.17.0.0/16"}]},"Options":{"com.docker.network.bridge.default_bridge":"true","com.docker.network.bridge.enable_icc":"true","com.docker.network.bridge.enable_ip_masquerade":"true","com.docker.network.bridge.host_binding_ipv4":"0.0.0.0","com.docker.network.bridge.name":"docker0","com.docker.network.driver.mtu":"1500"}},{"Name":"none","Id":"e086a3893b05ab69242d3c44e49483a3bbbd3a26b46baa8f61ab797c1088d794","Created":"0001-01-01T00:00:00Z","Scope":"local","Driver":"null","EnableIPv6":false,"Internal":false,"Attachable":false,"Ingress":false,"IPAM":{"Driver":"default","Config":[]},"Containers":{},"Options":{}},{"Name":"host","Id":"13e871235c677f196c4e1ecebb9dc733b9b2d2ab589e30c539efeda84a24215e","Created":"0001-01-01T00:00:00Z","Scope":"local","Driver":"host","EnableIPv6":false,"Internal":false,"Attachable":false,"Ingress":false,"IPAM":{"Driver":"default","Config":[]},"Containers":{},"Options":{}}]
data.frame(
  name = c("bridge", "none", "host"),
  id = c("f2de39df4171b0dc801e8002d1d999b77256983dfc63041c0f34030aa3977566",
         "e086a3893b05ab69242d3c44e49483a3bbbd3a26b46baa8f61ab797c1088d794",
         "13e871235c677f196c4e1ecebb9dc733b9b2d2ab589e30c539efeda84a24215e"),
  created = c("2016-10-19T06:21:00.416543526Z",
              "0001-01-01T00:00:00Z", "0001-01-01T00:00:00Z"),
  scope = "local",
  driver = c("bridge", "null", "host"),
  enable_ipv6 = FALSE,
  ipam = I(list(
    list(driver = "default",
         config = list(c(subnet = "172.17.0.0/16")),
         options = list()),
    list(driver = "default", config = list(),
         options = list()),
    list(driver = "default", config = list(),
         options = list()))),
  internal = FALSE,
  attachable = FALSE,
  ingress = FALSE,
  containers = I(list(list(), list(), list())),
  options = I(list(
    c(com.docker.network.bridge.default_bridge = "true",
      com.docker.network.bridge.enable_icc = "true",
      com.docker.network.bridge.enable_ip_masquerade = "true",
      com.docker.network.bridge.host_binding_ipv4 = "0.0.0.0",
      com.docker.network.bridge.name = "docker0",
      com.docker.network.driver.mtu = "1500"),
    character(),
    character())),
  labels = I(rep(list(character()), 3)),
  stringsAsFactors = FALSE)
