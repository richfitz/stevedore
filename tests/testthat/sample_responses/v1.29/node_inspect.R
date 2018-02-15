## version: 1.29
## method: get
## path: /nodes/{id}
## code: 200
## response: {"ID":"24ifsmvkjbyhk","Version":{"Index":8},"CreatedAt":"2016-06-07T20:31:11.853781916Z","UpdatedAt":"2016-06-07T20:31:11.999868824Z","Spec":{"Name":"my-node","Role":"manager","Availability":"active","Labels":{"foo":"bar"}},"Description":{"Hostname":"bf3067039e47","Platform":{"Architecture":"x86_64","OS":"linux"},"Resources":{"NanoCPUs":4000000000,"MemoryBytes":8272408576},"Engine":{"EngineVersion":"17.04.0","Labels":{"foo":"bar"},"Plugins":[{"Type":"Volume","Name":"local"},{"Type":"Network","Name":"bridge"},{"Type":"Network","Name":"null"},{"Type":"Network","Name":"overlay"}]}},"Status":{"State":"ready","Addr":"172.17.0.2"},"ManagerStatus":{"Leader":true,"Reachability":"reachable","Addr":"172.17.0.2:2377"}}
NULL

## NOTE: status and manager_status are not reported by this api
## version (but are present by 1.33)
list(
  id = "24ifsmvkjbyhk",
  version = list(index = 8L),
  created_at = "2016-06-07T20:31:11.853781916Z",
  updated_at = "2016-06-07T20:31:11.999868824Z",
  spec = list(
    name = "my-node",
    labels = c(foo = "bar"),
    role = "manager",
    availability = "active"),
  description = list(
    hostname = "bf3067039e47",
    platform = list(
      architecture = "x86_64",
      os = "linux"),
    resources = list(
      nano_cpus = 4e+09,
      memory_bytes = 8272408576),
    engine = list(
      engine_version = "17.04.0",
      labels = c(foo = "bar"),
      plugins = data.frame(
        type = c("Volume", "Network", "Network", "Network"),
        name = c("local",  "bridge",  "null",    "overlay"),
        stringsAsFactors = FALSE))))
