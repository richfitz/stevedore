<!-- -*-markdown-*- -->
# stevedore

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/stevedore.svg?branch=master)](https://travis-ci.org/richfitz/stevedore)
[![codecov.io](https://codecov.io/github/richfitz/stevedore/coverage.svg?branch=master)](https://codecov.io/github/richfitz/stevedore?branch=master)

A docker client for R

**WARNING**: package is in early development. If you run the test suite it may do all sorts of things to your containers/images/volumes/networks.  Do not run it unless you'd be happy running `docker system prune -f`



## Usage

The main function in the package is `docker_client`; this will construct an object with which we can talk with the docker server.


```r
docker <- stevedore::docker_client()
docker
```

```
## <docker_client>
##   containers: docker_container_collection
##   images: docker_image_collection
##   networks: docker_network_collection
##   volumes: docker_volume_collection
##   api_version()
##   df()
##   events(since = NULL, until = NULL, filters = NULL)
##   info()
##   login(username = NULL, password = NULL, email = NULL,
##       serveraddress = NULL)
##   ping()
##   version()
```

With this you can run containers:


```r
docker$containers$run("alpine:3.1", c("echo", "hello world"))
```

```
## Unable to find image 'alpine:3.1' locally
```

```
## Pulling from library/alpine 3.1
## Pulling fs layer 61aa778aed31
## 61aa778aed31: Downloading 32.77 kB/2.3 MB 1%
## Verifying Checksum 61aa778aed31
## Download complete 61aa778aed31
## 61aa778aed31: Extracting 32.77 kB/2.3 MB 1%
## 61aa778aed31: Extracting 557.06 kB/2.3 MB 24%
## 61aa778aed31: Extracting 2.3 MB/2.3 MB 100%
## Pull complete 61aa778aed31
## Digest: sha256:10de714727daa45047abdfb81c98dbf45e1cad3b590b5043d0da139bfeacebe5
## Status: Downloaded newer image for alpine:3.1
```

```
## $container
## <docker_container>
##   commit(hostname = NULL, domainname = NULL, user = NULL,
##       attach_stdin = NULL, attach_stdout = NULL, attach_stderr = NULL,
##       exposed_ports = NULL, tty = NULL, open_stdin = NULL,
##       stdin_once = NULL, env = NULL, cmd = NULL, healthcheck = NULL,
##       args_escaped = NULL, image = NULL, volumes = NULL,
##       working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##       mac_address = NULL, on_build = NULL, labels = NULL,
##       stop_signal = NULL, stop_timeout = NULL, shell = NULL,
##       container = NULL, repo = NULL, tag = NULL, comment = NULL,
##       author = NULL, pause = NULL, changes = NULL)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL)
##   export()
##   get_archive(path, dest)
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels()
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE,
##       since = NULL, timestamps = NULL, tail = NULL, stream = stdout())
##   name()
##   path_stat(path)
##   pause()
##   ports(reload = TRUE)
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status(reload = TRUE)
##   stop(t = NULL)
##   top(ps_args = NULL)
##   unpause()
##   update(cpu_shares = NULL, memory = NULL, cgroup_parent = NULL,
##       blkio_weight = NULL, blkio_weight_device = NULL,
##       blkio_device_read_bps = NULL, blkio_device_write_bps = NULL,
##       blkio_device_read_iops = NULL, blkio_device_write_iops = NULL,
##       cpu_period = NULL, cpu_quota = NULL, cpu_realtime_period = NULL,
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL,
##       cpuset_mems = NULL, devices = NULL, device_cgroup_rules = NULL,
##       disk_quota = NULL, kernel_memory = NULL, memory_reservation = NULL,
##       memory_swap = NULL, memory_swappiness = NULL, nano_cpus = NULL,
##       oom_kill_disable = NULL, pids_limit = NULL, ulimits = NULL,
##       cpu_count = NULL, cpu_percent = NULL, io_maximum_iops = NULL,
##       io_maximum_bandwidth = NULL, restart_policy = NULL)
##   wait()
##
## $logs
## O> hello world
```

Or run containers in the background


```r
docker$containers$run("bfirsh/reticulate-splines", detach = TRUE)
```

```
## <docker_container>
##   commit(hostname = NULL, domainname = NULL, user = NULL,
##       attach_stdin = NULL, attach_stdout = NULL, attach_stderr = NULL,
##       exposed_ports = NULL, tty = NULL, open_stdin = NULL,
##       stdin_once = NULL, env = NULL, cmd = NULL, healthcheck = NULL,
##       args_escaped = NULL, image = NULL, volumes = NULL,
##       working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##       mac_address = NULL, on_build = NULL, labels = NULL,
##       stop_signal = NULL, stop_timeout = NULL, shell = NULL,
##       container = NULL, repo = NULL, tag = NULL, comment = NULL,
##       author = NULL, pause = NULL, changes = NULL)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL)
##   export()
##   get_archive(path, dest)
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels()
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE,
##       since = NULL, timestamps = NULL, tail = NULL, stream = stdout())
##   name()
##   path_stat(path)
##   pause()
##   ports(reload = TRUE)
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status(reload = TRUE)
##   stop(t = NULL)
##   top(ps_args = NULL)
##   unpause()
##   update(cpu_shares = NULL, memory = NULL, cgroup_parent = NULL,
##       blkio_weight = NULL, blkio_weight_device = NULL,
##       blkio_device_read_bps = NULL, blkio_device_write_bps = NULL,
##       blkio_device_read_iops = NULL, blkio_device_write_iops = NULL,
##       cpu_period = NULL, cpu_quota = NULL, cpu_realtime_period = NULL,
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL,
##       cpuset_mems = NULL, devices = NULL, device_cgroup_rules = NULL,
##       disk_quota = NULL, kernel_memory = NULL, memory_reservation = NULL,
##       memory_swap = NULL, memory_swappiness = NULL, nano_cpus = NULL,
##       oom_kill_disable = NULL, pids_limit = NULL, ulimits = NULL,
##       cpu_count = NULL, cpu_percent = NULL, io_maximum_iops = NULL,
##       io_maximum_bandwidth = NULL, restart_policy = NULL)
##   wait()
```

You can manage containers


```r
docker$containers$list()
```

```
##                                                                 id
## 1 4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5
## 2 ac1893fcccf4a6536bfea6f02bd6adc8cac11879921884a667432a7cbc0c5b85
##          names
## 1 competen....
## 2 stevedor....
##                                                                     image
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2                                                        richfitz/iterate
##                                                                  image_id
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2 sha256:0192b2d0deee7f8a7cebf51f51a83c08e84735bf28ba12ff7199eced6f5b6966
##                          command    created        ports size_rw
## 1          /usr/local/bin/run.sh 1515668933 characte....      NA
## 2 /usr/local/bin/iterate 100 100 1515668500 characte....      NA
##   size_root_fs labels   state                status host_config
## 1           NA        running Up Less than a second     default
## 2           NA  0.0.1 running          Up 7 minutes     default
##   network_settings       mounts                 name
## 1     list(bri.... characte....       competent_wing
## 2     list(bri.... c(NA, NA.... stevedore_zgxyvlmhib
```

```r
id <- docker$containers$list(limit = 1L)$id
container <- docker$containers$get(id)
container
```

```
## <docker_container>
##   commit(hostname = NULL, domainname = NULL, user = NULL,
##       attach_stdin = NULL, attach_stdout = NULL, attach_stderr = NULL,
##       exposed_ports = NULL, tty = NULL, open_stdin = NULL,
##       stdin_once = NULL, env = NULL, cmd = NULL, healthcheck = NULL,
##       args_escaped = NULL, image = NULL, volumes = NULL,
##       working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##       mac_address = NULL, on_build = NULL, labels = NULL,
##       stop_signal = NULL, stop_timeout = NULL, shell = NULL,
##       container = NULL, repo = NULL, tag = NULL, comment = NULL,
##       author = NULL, pause = NULL, changes = NULL)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL)
##   export()
##   get_archive(path, dest)
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels()
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE,
##       since = NULL, timestamps = NULL, tail = NULL, stream = stdout())
##   name()
##   path_stat(path)
##   pause()
##   ports(reload = TRUE)
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status(reload = TRUE)
##   stop(t = NULL)
##   top(ps_args = NULL)
##   unpause()
##   update(cpu_shares = NULL, memory = NULL, cgroup_parent = NULL,
##       blkio_weight = NULL, blkio_weight_device = NULL,
##       blkio_device_read_bps = NULL, blkio_device_write_bps = NULL,
##       blkio_device_read_iops = NULL, blkio_device_write_iops = NULL,
##       cpu_period = NULL, cpu_quota = NULL, cpu_realtime_period = NULL,
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL,
##       cpuset_mems = NULL, devices = NULL, device_cgroup_rules = NULL,
##       disk_quota = NULL, kernel_memory = NULL, memory_reservation = NULL,
##       memory_swap = NULL, memory_swappiness = NULL, nano_cpus = NULL,
##       oom_kill_disable = NULL, pids_limit = NULL, ulimits = NULL,
##       cpu_count = NULL, cpu_percent = NULL, io_maximum_iops = NULL,
##       io_maximum_bandwidth = NULL, restart_policy = NULL)
##   wait()
```

And control containers


```r
container$inspect()$config$image
```

```
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
```

```r
container$logs()
```

```
## O> Reticulating spline 1...
```

```r
container$stop(t = 0)
```

```
## NULL
```

```r
container$remove()
```

```
## NULL
```

And manage images


```r
head(docker$images$list())
```

```
##                                                                        id
## 1 sha256:609e673f3745b7d53d0f3e361c1b76640b83a681a8f20cc13d000a0c1bdfc814
## 2 sha256:9c3933c2060224a6e939118f7ce9c1737ec6b43dafab66b0ebfbcc0f2e52852d
## 3 sha256:8158ab4f28ef06631efaf8fd02388002bb9ad05091883d79b235fdac7c206d15
## 4 sha256:fc930d4e25c4856ff86be256e79fa17e74d135ec52dd4222f01e00b8308b97af
## 5 sha256:28262efd50a4ce9e74ad5a75cef6b8e30ddf449436df636c29c902eed40e3342
## 6 sha256:e5d735f8cb4df6b2287876bdf22d17129beac7b5512af4a2ca72a79b75867202
##                                                                 parent_id
## 1 sha256:d4537a978ae83cf35ca20190f29bef9e02edb03b2eef057ba5176efd4f3964f2
## 2 sha256:12f747117f6c72a21053dc1d5b5e7bde7364aada18d7244fa12274592e03e2c2
## 3 sha256:30ee878b4acfd1a351a1614894f39a87107cdc2db72535b64bd8ffd93ec12382
## 4 sha256:e262c6148455e7079ff714220d677fcd63856df8935eeb12415d52ca0763988f
## 5 sha256:c4fd57f84b6d3349e1ae63ed6f6b770219d72ae70bcfd9294c8b82aa13fb75c7
## 6 sha256:22278ebf5ddac24a964594ff555e26ffbbcb0a94df0ae20a115fcc0523b1d822
##      repo_tags repo_digests    created    size shared_size virtual_size
## 1 richfitz....              1515668785 4143990          -1      4143990
## 2 <none>:<.... <none>@<.... 1515668784 4143990          -1      4143990
## 3 <none>:<.... <none>@<.... 1515668783 4143990          -1      4143990
## 4 <none>:<.... <none>@<.... 1515668781 4143990          -1      4143990
## 5 <none>:<.... <none>@<.... 1515668699 4143990          -1      4143990
## 6 <none>:<.... <none>@<.... 1515668698 4143990          -1      4143990
##   labels containers
## 1  0.0.1         -1
## 2  0.0.1         -1
## 3  0.0.1         -1
## 4  0.0.1         -1
## 5  0.0.1         -1
## 6  0.0.1         -1
```

## Approach

Docker publishes a [machine-readable API specification](https://docs.docker.com/engine/api/v1.29).  Rather than manually write wrappers that fit the output docker gives, `stevedore` _generates_ an interface directly from the spefification.  Currently `stevedore` supports docker API versions 1.25 to 1.25 (defaulting to 1.29).

This approach means that the output will be type-stable - there is no inference on what to return based on what the server chooses to return.  With a given API version, the same fields will always be returned.  Some of this information is very rich, for example, for the backgrounded container above:


```r
container$inspect(reload = FALSE)
```

```
## $id
## [1] "4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5"
##
## $created
## [1] "2018-01-11T11:08:53.316577922Z"
##
## $path
## [1] "/usr/local/bin/run.sh"
##
## $args
## character(0)
##
## $state
## $state$status
## [1] "running"
##
## $state$running
## [1] TRUE
##
## $state$paused
## [1] FALSE
##
## $state$restarting
## [1] FALSE
##
## $state$oom_killed
## [1] FALSE
##
## $state$dead
## [1] FALSE
##
## $state$pid
## [1] 27836
##
## $state$exit_code
## [1] 0
##
## $state$error
## [1] ""
##
## $state$started_at
## [1] "2018-01-11T11:08:53.929100207Z"
##
## $state$finished_at
## [1] "0001-01-01T00:00:00Z"
##
##
## $image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $resolv_conf_path
## [1] "/var/lib/docker/containers/4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5/resolv.conf"
##
## $hostname_path
## [1] "/var/lib/docker/containers/4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5/hostname"
##
## $hosts_path
## [1] "/var/lib/docker/containers/4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5/hosts"
##
## $log_path
## [1] "/var/lib/docker/containers/4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5/4d73187948328870f7d3e5b29d8de85adfc1f8b841c6acbaa07a32933488a8d5-json.log"
##
## $node
## NULL
##
## $name
## [1] "/competent_wing"
##
## $restart_count
## [1] 0
##
## $driver
## [1] "overlay2"
##
## $mount_label
## [1] ""
##
## $process_label
## [1] ""
##
## $app_armor_profile
## [1] ""
##
## $exec_ids
## [1] NA
##
## $host_config
## $host_config$cpu_shares
## [1] 0
##
## $host_config$memory
## [1] 0
##
## $host_config$cgroup_parent
## [1] ""
##
## $host_config$blkio_weight
## [1] 0
##
## $host_config$blkio_weight_device
## [1] path   weight
## <0 rows> (or 0-length row.names)
##
## $host_config$blkio_device_read_bps
## [1] path rate
## <0 rows> (or 0-length row.names)
##
## $host_config$blkio_device_write_bps
## [1] path rate
## <0 rows> (or 0-length row.names)
##
## $host_config$blkio_device_read_iops
## [1] path rate
## <0 rows> (or 0-length row.names)
##
## $host_config$blkio_device_write_iops
## [1] path rate
## <0 rows> (or 0-length row.names)
##
## $host_config$cpu_period
## [1] 0
##
## $host_config$cpu_quota
## [1] 0
##
## $host_config$cpu_realtime_period
## [1] 0
##
## $host_config$cpu_realtime_runtime
## [1] 0
##
## $host_config$cpuset_cpus
## [1] ""
##
## $host_config$cpuset_mems
## [1] ""
##
## $host_config$devices
## [1] path_on_host       path_in_container  cgroup_permissions
## <0 rows> (or 0-length row.names)
##
## $host_config$device_cgroup_rules
## character(0)
##
## $host_config$disk_quota
## [1] 0
##
## $host_config$kernel_memory
## [1] 0
##
## $host_config$memory_reservation
## [1] 0
##
## $host_config$memory_swap
## [1] 0
##
## $host_config$memory_swappiness
## [1] NA
##
## $host_config$nano_cpus
## [1] NA
##
## $host_config$oom_kill_disable
## [1] FALSE
##
## $host_config$pids_limit
## [1] 0
##
## $host_config$ulimits
## [1] name soft hard
## <0 rows> (or 0-length row.names)
##
## $host_config$cpu_count
## [1] 0
##
## $host_config$cpu_percent
## [1] 0
##
## $host_config$io_maximum_iops
## [1] 0
##
## $host_config$io_maximum_bandwidth
## [1] 0
##
## $host_config$binds
## character(0)
##
## $host_config$container_idfile
## [1] ""
##
## $host_config$log_config
## $host_config$log_config$type
## [1] "json-file"
##
## $host_config$log_config$config
## character(0)
##
##
## $host_config$network_mode
## [1] "default"
##
## $host_config$port_bindings
## NULL
##
## $host_config$restart_policy
## $host_config$restart_policy$name
## [1] ""
##
## $host_config$restart_policy$maximum_retry_count
## [1] 0
##
##
## $host_config$auto_remove
## [1] FALSE
##
## $host_config$volume_driver
## [1] ""
##
## $host_config$volumes_from
## character(0)
##
## $host_config$mounts
## [1] target         source         type           read_only
## [5] consistency    bind_options   volume_options tmpfs_options
## <0 rows> (or 0-length row.names)
##
## $host_config$cap_add
## character(0)
##
## $host_config$cap_drop
## character(0)
##
## $host_config$dns
## character(0)
##
## $host_config$dns_options
## character(0)
##
## $host_config$dns_search
## character(0)
##
## $host_config$extra_hosts
## character(0)
##
## $host_config$group_add
## character(0)
##
## $host_config$ipc_mode
## [1] "shareable"
##
## $host_config$cgroup
## [1] ""
##
## $host_config$links
## character(0)
##
## $host_config$oom_score_adj
## [1] 0
##
## $host_config$pid_mode
## [1] ""
##
## $host_config$privileged
## [1] FALSE
##
## $host_config$publish_all_ports
## [1] FALSE
##
## $host_config$readonly_rootfs
## [1] FALSE
##
## $host_config$security_opt
## character(0)
##
## $host_config$storage_opt
## NULL
##
## $host_config$tmpfs
## NULL
##
## $host_config$uts_mode
## [1] ""
##
## $host_config$userns_mode
## [1] ""
##
## $host_config$shm_size
## [1] 67108864
##
## $host_config$sysctls
## NULL
##
## $host_config$runtime
## [1] "runc"
##
## $host_config$console_size
## [1] 0 0
##
## $host_config$isolation
## [1] ""
##
##
## $graph_driver
## $graph_driver$name
## [1] "overlay2"
##
## $graph_driver$data
##                                                                                                                                                                                                                                                                                                                                                                                          lower_dir
## "/var/lib/docker/overlay2/ff7801caecc598d6f6514adc5dbbd1cb7acb3c96ed291e886e04028a5c25ca07-init/diff:/var/lib/docker/overlay2/7815d9bac55ed4ceb36bd625979dfcd06330e8253b9f71c95795ce5c5b247dd0/diff:/var/lib/docker/overlay2/2b72161d0aefe066769d9334de310b36616ae170f4472a7384324d378dd82cb5/diff:/var/lib/docker/overlay2/d4903c54aa9b839529e6b24e2293abe7cbea0093a5106726c0e93754cb105591/diff"
##                                                                                                                                                                                                                                                                                                                                                                                         merged_dir
##                                                                                                                                                                                                                                                                                                 "/var/lib/docker/overlay2/ff7801caecc598d6f6514adc5dbbd1cb7acb3c96ed291e886e04028a5c25ca07/merged"
##                                                                                                                                                                                                                                                                                                                                                                                          upper_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/ff7801caecc598d6f6514adc5dbbd1cb7acb3c96ed291e886e04028a5c25ca07/diff"
##                                                                                                                                                                                                                                                                                                                                                                                           work_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/ff7801caecc598d6f6514adc5dbbd1cb7acb3c96ed291e886e04028a5c25ca07/work"
##
##
## $size_rw
## [1] NA
##
## $size_root_fs
## [1] NA
##
## $mounts
## [1] type        name        source      destination driver      mode
## [7] rw          propagation
## <0 rows> (or 0-length row.names)
##
## $config
## $config$hostname
## [1] "4d7318794832"
##
## $config$domainname
## [1] ""
##
## $config$user
## [1] ""
##
## $config$attach_stdin
## [1] FALSE
##
## $config$attach_stdout
## [1] FALSE
##
## $config$attach_stderr
## [1] FALSE
##
## $config$exposed_ports
## NULL
##
## $config$tty
## [1] FALSE
##
## $config$open_stdin
## [1] FALSE
##
## $config$stdin_once
## [1] FALSE
##
## $config$env
## [1] "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
##
## $config$cmd
## [1] "/usr/local/bin/run.sh"
##
## $config$healthcheck
## NULL
##
## $config$args_escaped
## [1] TRUE
##
## $config$image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $config$volumes
## NULL
##
## $config$working_dir
## [1] ""
##
## $config$entrypoint
## character(0)
##
## $config$network_disabled
## [1] NA
##
## $config$mac_address
## [1] NA
##
## $config$on_build
## character(0)
##
## $config$labels
## character(0)
##
## $config$stop_signal
## [1] NA
##
## $config$stop_timeout
## [1] NA
##
## $config$shell
## character(0)
##
##
## $network_settings
## $network_settings$bridge
## [1] ""
##
## $network_settings$gateway
## [1] "172.17.0.1"
##
## $network_settings$address
## [1] NA
##
## $network_settings$ip_prefix_len
## [1] 16
##
## $network_settings$mac_address
## [1] "02:42:ac:11:00:03"
##
## $network_settings$port_mapping
## [1] NA
##
## $network_settings$ports
## list()
```

## Roadmap

There is still a lot of work to do here:

* windows support needs work - the current approach works only for a unix socket
* unix non-socket (tcp) access, and TLS
* endpoints that require http hijacking are not fully supported (i.e., attach) but the foundations are there to support this - stdin is likely to be a major hassle though and I'm not sure if it's possible from within R's REPL
* endpoints that require tar input and output (equivalents of `docker cp` especially) need major work
* lots of work on parameter wrangling for the more complicated endpoints (basically things that take anything more complicated than a string array are prone to failure because I've not tested them yet)
* swarm features (`nodes`, `plugins`, `secrets`, `services` and `swarm`) are not implemented - not because they'd be any harder but just because I've never used them
* authentication of registries for pull and push (not hard, just not done)
* some sort of documentation - hard given that the interface is directly pulled from the spec so we need to patch up references to json

## Licence

MIT © [Rich FitzJohn](https://github.com/richfitz).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
