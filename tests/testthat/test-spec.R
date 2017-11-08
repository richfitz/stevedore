context("sample responses")

dat <- docker_client_data("v1.29")

describe_api(dat)

run_sample_responses(docker_client_data("v1.29"))
run_sample_responses(docker_client_data("v1.30"))

run_sample_response(dat$endpoints$system_ping, dat$spec)
run_sample_response(dat$endpoints$system_auth, dat$spec)

## I'd like some nice examples of different behaviours in the
## examples, from the most simple to the most complex.
run_sample_responses(docker_client_data("v1.29"))

dat_1_29 <- docker_client_data("v1.29")

test_that("atomic, scalar", {
  ## the most simple case; GET /_ping : 200 is string -> string
  h <- dat_1_29$endpoints$system_ping$response_handlers[["200"]]
  expect_equal(h("OK", FALSE), "OK")
})

test_that("object, atomic scalar components", {
  str <- '{"Status":"Login Succeeded","IdentityToken":"9cbaf023786cd7..."}'
  h <- dat_1_29$endpoints$system_auth$response_handlers[["200"]]
  res <- h(charToRaw(str))
  expect_equal(res, list(status = "Login Succeeded",
                         identity_token = "9cbaf023786cd7..."))

  res2 <- h(charToRaw(str), as_is_names = TRUE)
  expect_equal(unname(res), unname(res2))
  expect_equal(names(res2), c("Status", "IdentityToken"))
})

test_that("object, array of array", {
  dat_1_29 <- docker_client_data("v1.29")
  ## jsonlite::toJSON(dat_1_29$spec$paths[["/containers/{id}/top"]]$get$responses[["200"]]$examples[[1]])
  str <- '{"Titles":["UID","PID","PPID","C","STIME","TTY","TIME","CMD"],"Processes":[["root","13642","882","0","17:03","pts/0","00:00:00","/bin/bash"],["root","13735","13642","0","17:06","pts/0","00:00:00","sleep 10"]]}'
  h <- dat_1_29$endpoints$container_top$response_handlers[["200"]]
  ans <- h(charToRaw(str))
  ans2 <- h(charToRaw(str), as_is_names = TRUE)

  cmp <- jsonlite::fromJSON(str, simplifyMatrix = FALSE)
  names(cmp) <- tolower(names(cmp))

  expect_equal(ans, cmp)
  expect_equal(unname(ans2), unname(cmp))
  expect_equal(names(ans2), c("Titles", "Processes"))
})

test_that("object with objects", {
  ## dat_1_29$spec$paths[["/volumes/{name}"]]$get$responses[["200"]]
  ## jsonlite::toJSON(dat_1_29$spec$definitions$Volume$example,
  ##                  auto_unbox = TRUE)
  str <- {"Name":"tardis","Driver":"custom","Mountpoint":"/var/lib/docker/volumes/tardis","Status":{"hello":"world"},"Labels":{"com.example.some-label":"some-value","com.example.some-other-label":"some-other-value"},"Scope":"local"}

  ## Totally manually - probably the way to do all of this, actually -
  ## consider breaking up all the tests like this, because then we can
  ## just read the spec and generate these individually.
  spec <- dat_1_29$spec
  schema <- spec$paths$"/volumes/{name}"$get$responses[["200"]]
  h <- make_response_handler(schema, spec)
  ans <- h(charToRaw(str))
  ans2 <- h(charToRaw(str), as_is_names = TRUE)

  cmp <- list(name = "tardis",
              driver = "custom",
              mountpoint = "/var/lib/docker/volumes/tardis",
              status = list(hello = list("world")),
              labels = list("com.example.some-label" = "some-value",
                            "com.example.some-other-label" =
                              "some-other-value"),
              scope = "local",
              options = NULL,
              usage_data = NULL)
  expect_equal(cmp, ans)
  expect_equal(unname(cmp), unname(ans2))
  expect_equal(names(ans2), c("Name", "Driver", "Mountpoint", "Status",
                              "Labels", "Scope", "Options", "UsageData"))
})

test_that("object with objects", {
  ## 'GET /containers/json' - large but complete
  ## 'GET /containers/{id}/json' - slightly smaller perhaps?
  spec_1_29 <- read_spec("v1.29")

  schema <- spec_1_29$paths$"/containers/{id}/json"$get$responses[["200"]]

  ## jsonlite::toJSON(schema$examples[[1]], auto_unbox = TRUE)
  str <- '{"AppArmorProfile":"","Args":["-c","exit 9"],"Config":{"AttachStderr":true,"AttachStdin":false,"AttachStdout":true,"Cmd":["/bin/sh","-c","exit 9"],"Domainname":"","Env":"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin","Hostname":"ba033ac44011","Image":"ubuntu","Labels":{"com.example.vendor":"Acme","com.example.license":"GPL","com.example.version":"1.0"},"MacAddress":"","NetworkDisabled":false,"OpenStdin":false,"StdinOnce":false,"Tty":false,"User":"","Volumes":{"/volumes/data":{}},"WorkingDir":"","StopSignal":"SIGTERM","StopTimeout":10},"Created":"2015-01-06T15:47:31.485331387Z","Driver":"devicemapper","HostConfig":{"MaximumIOps":0,"MaximumIOBps":0,"BlkioWeight":0,"BlkioWeightDevice":[{}],"BlkioDeviceReadBps":[{}],"BlkioDeviceWriteBps":[{}],"BlkioDeviceReadIOps":[{}],"BlkioDeviceWriteIOps":[{}],"ContainerIDFile":"","CpusetCpus":"","CpusetMems":"","CpuPercent":80,"CpuShares":0,"CpuPeriod":100000,"CpuRealtimePeriod":1000000,"CpuRealtimeRuntime":10000,"Devices":[],"IpcMode":"","LxcConf":[],"Memory":0,"MemorySwap":0,"MemoryReservation":0,"KernelMemory":0,"OomKillDisable":false,"OomScoreAdj":500,"NetworkMode":"bridge","PidMode":"","PortBindings":{},"Privileged":false,"ReadonlyRootfs":false,"PublishAllPorts":false,"RestartPolicy":{"MaximumRetryCount":2,"Name":"on-failure"},"LogConfig":{"Type":"json-file"},"Sysctls":{"net.ipv4.ip_forward":"1"},"Ulimits":[{}],"VolumeDriver":"","ShmSize":67108864},"HostnamePath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hostname","HostsPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/hosts","LogPath":"/var/lib/docker/containers/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b/1eb5fabf5a03807136561b3c00adcd2992b535d624d5e18b6cdc6a6844d9767b-json.log","Id":"ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39","Image":"04c5d3b7b0656168630d3ba35d8889bd0e9caafcaeb3004d2bfbc47e7c5d35d2","MountLabel":"","Name":"/boring_euclid","NetworkSettings":{"Bridge":"","SandboxID":"","HairpinMode":false,"LinkLocalIPv6Address":"","LinkLocalIPv6PrefixLen":0,"SandboxKey":"","SecondaryIPAddresses":{},"SecondaryIPv6Addresses":{},"EndpointID":"","Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"","MacAddress":"","Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"7587b82f0dada3656fda26588aee72630c6fab1536d36e394b2bfbcf898c971d","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:12:00:02"}}},"Path":"/bin/sh","ProcessLabel":"","ResolvConfPath":"/var/lib/docker/containers/ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39/resolv.conf","RestartCount":1,"State":{"Error":"","ExitCode":9,"FinishedAt":"2015-01-06T15:47:32.080254511Z","OOMKilled":false,"Dead":false,"Paused":false,"Pid":0,"Restarting":false,"Running":true,"StartedAt":"2015-01-06T15:47:32.072697474Z","Status":"running"},"Mounts":[{"Name":"fac362...80535","Source":"/data","Destination":"/data","Driver":"local","Mode":"ro,Z","RW":false,"Propagation":""}]}'

  h <- make_response_handler(schema, spec_1_29)
  ans <- h(charToRaw(str))
  ans2 <- h(charToRaw(str), as_is_names = TRUE)

})
