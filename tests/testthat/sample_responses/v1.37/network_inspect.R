## version: 1.37
## method: get
## path: /networks/{id}
## code: 200
## response: {"Name":"net01","Id":"7d86d31b1478e7cca9ebed7e73aa0fdeec46c5ca29497431d3007d2d9e15ed99","Created":"2016-10-19T04:33:30.360899459Z","Scope":"local","Driver":"bridge","EnableIPv6":false,"IPAM":{"Driver":"default","Config":[{"Subnet":"172.19.0.0/16","Gateway":"172.19.0.1"}],"Options":{"foo":"bar"}},"Internal":false,"Attachable":false,"Ingress":false,"Containers":{"19a4d5d687db25203351ed79d478946f861258f018fe384f229f2efa4b23513c":{"Name":"test","EndpointID":"628cadb8bcb92de107b2a1e516cbffe463e321f548feb37697cce00ad694f21a","MacAddress":"02:42:ac:13:00:02","IPv4Address":"172.19.0.2/16","IPv6Address":""}},"Options":{"com.docker.network.bridge.default_bridge":"true","com.docker.network.bridge.enable_icc":"true","com.docker.network.bridge.enable_ip_masquerade":"true","com.docker.network.bridge.host_binding_ipv4":"0.0.0.0","com.docker.network.bridge.name":"docker0","com.docker.network.driver.mtu":"1500"},"Labels":{"com.example.some-label":"some-value","com.example.some-other-label":"some-other-value"}}
containers <- list(
  name = "test",
  endpoint_id = "628cadb8bcb92de107b2a1e516cbffe463e321f548feb37697cce00ad694f21a",
  mac_address = "02:42:ac:13:00:02",
  ipv4_address = "172.19.0.2/16",
  ipv6_address = "")
containers <- list(
  "19a4d5d687db25203351ed79d478946f861258f018fe384f229f2efa4b23513c" =
    containers)

labels <- c("com.example.some-label" = "some-value",
            "com.example.some-other-label" = "some-other-value")

options <- c(
  com.docker.network.bridge.default_bridge = "true",
  com.docker.network.bridge.enable_icc = "true",
  com.docker.network.bridge.enable_ip_masquerade = "true",
  com.docker.network.bridge.host_binding_ipv4 = "0.0.0.0",
  com.docker.network.bridge.name = "docker0",
  com.docker.network.driver.mtu = "1500")

ipam <- list(
  driver = "default",
  config = list(c(subnet = "172.19.0.0/16", gateway = "172.19.0.1")),
  options = list(foo = c("bar" = "bar")))

list(
  name = "net01",
  id = "7d86d31b1478e7cca9ebed7e73aa0fdeec46c5ca29497431d3007d2d9e15ed99",
  created = "2016-10-19T04:33:30.360899459Z",
  scope = "local",
  driver = "bridge",
  enable_ipv6 = FALSE,
  ipam = ipam,
  internal = FALSE,
  attachable = FALSE,
  ingress = FALSE,
  containers = containers,
  options = options,
  labels = labels)
