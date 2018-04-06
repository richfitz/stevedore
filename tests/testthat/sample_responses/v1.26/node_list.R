## version: 1.26
## method: get
## path: /nodes
## code: 200
## response: [{"ID":"24ifsmvkjbyhk","Version":{"Index":8},"CreatedAt":"2016-06-07T20:31:11.853781916Z","UpdatedAt":"2016-06-07T20:31:11.999868824Z","Spec":{"Name":"my-node","Role":"manager","Availability":"active","Labels":{"foo":"bar"}},"Description":{"Hostname":"bf3067039e47","Platform":{"Architecture":"x86_64","OS":"linux"},"Resources":{"NanoCPUs":4000000000,"MemoryBytes":8272408576},"Engine":{"EngineVersion":"1.13.0","Labels":{"foo":"bar"},"Plugins":[{"Type":"Volume","Name":"local"},{"Type":"Network","Name":"bridge"},{"Type":"Network","Name":"null"},{"Type":"Network","Name":"overlay"}]}},"Status":{"State":"ready","Addr":"172.17.0.2"},"ManagerStatus":{"Leader":true,"Reachability":"reachable","Addr":"172.17.0.2:2377"}}]
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

data_frame(
  id = "24ifsmvkjbyhk",
  version = I(list(list(index = 8L))),
  created_at = "2016-06-07T20:31:11.853781916Z",
  updated_at = "2016-06-07T20:31:11.999868824Z",
  spec = I(list(list(
    name = "my-node",
    labels = c(foo = "bar"),
    role = "manager",
    availability = "active"))),
  description = I(list(list(
    hostname = "bf3067039e47",
    platform = list(
      architecture = "x86_64",
      os = "linux"),
    resources = list(
      nano_cpus = 4e+09,
      memory_bytes = 8272408576),
    engine = list(
      engine_version = "1.13.0",
      labels = c(foo = "bar"),
      plugins = data.frame(
        type = c("Volume", "Network", "Network", "Network"),
        name = c("local",  "bridge",  "null",    "overlay"),
        stringsAsFactors = FALSE))))),
  status = I(list(list(
    state = "ready",
    message = NA_character_,
    addr = "172.17.0.2"))),
  manager_status = I(list(list(
    leader = TRUE,
    reachability = "reachable",
    addr = "172.17.0.2:2377"))))
