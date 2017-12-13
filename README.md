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
## Pulling fs layer e823343fc78a
## e823343fc78a: Downloading 32.77 kB/2.3 MB 1%
## e823343fc78a: Downloading 1.33 MB/2.3 MB 58%
## Download complete e823343fc78a
## e823343fc78a: Extracting 32.77 kB/2.3 MB 1%
## e823343fc78a: Extracting 557.06 kB/2.3 MB 24%
## e823343fc78a: Extracting 2.3 MB/2.3 MB 100%
## Pull complete e823343fc78a
## Digest: sha256:b93469816c9f92b802c70c38829ae899c232b5d8bcedaf91fb4df83b82427add
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
##   logs(stdout = TRUE, stderr = TRUE, since = NULL,
##       timestamps = NULL, tail = NULL)
##   name()
##   path_stat(path)
##   pause()
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status()
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
##   logs(stdout = TRUE, stderr = TRUE, since = NULL,
##       timestamps = NULL, tail = NULL)
##   name()
##   path_stat(path)
##   pause()
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status()
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
## 1 54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235
##          names
## 1 admiring....
##                                                                     image
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
##                                                                  image_id
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
##                 command    created        ports size_rw size_root_fs
## 1 /usr/local/bin/run.sh 1513163999 characte....      NA           NA
##   labels   state                status host_config network_settings
## 1        running Up Less than a second     default     list(bri....
##         mounts           name
## 1 characte.... admiring_booth
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
##   logs(stdout = TRUE, stderr = TRUE, since = NULL,
##       timestamps = NULL, tail = NULL)
##   name()
##   path_stat(path)
##   pause()
##   put_archive(input_stream, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(v = NULL, force = NULL, link = NULL)
##   rename(name)
##   resize(h = NULL, w = NULL)
##   restart(t = NULL)
##   start(detach_keys = NULL)
##   stats()
##   status()
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
## 1 sha256:45911c9130ae0ab50a9e1f43c40b881f6fef9428b667d67ee53a0182886e4187
## 2 sha256:f2a91732366c0332ccd7afd2a5c4ff2b9af81f549370f7a19acd460f87686bc7
## 3 sha256:93fd6b1bc1dcc402e51c51209c4899384541613c10b91546195c70f35fb28931
## 4 sha256:20c44cd7596ff4807aef84273c99588d22749e2a7e15a7545ac96347baa65eda
## 5 sha256:18d8d3ea70507fd11abb87c56656676f549d0f24ebf8efa57101d624d03d0136
## 6 sha256:17e4d672aa7968bce4b05294b2c243bebff2ade7ebb14c5c749684d6a81fa2a5
##   parent_id    repo_tags repo_digests    created      size shared_size
## 1             alpine:3.1 alpine@s.... 1512154151   5046830          -1
## 2           hello-wo....              1511223798      1848          -1
## 3           ubuntu:17.04 ubuntu@s.... 1510955978  95437822          -1
## 4           ubuntu:16.04 ubuntu@s.... 1510955965 122792927          -1
## 5           crate:latest crate@sh.... 1502734455 204527823          -1
## 6           docker.m.... docker.m.... 1499245251 887797278          -1
##   virtual_size labels containers
## 1      5046830                -1
## 2         1848                -1
## 3     95437822                -1
## 4    122792927                -1
## 5    204527823                -1
## 6    887797278                -1
```

## Approach

Docker publishes a [machine-readable API specification](https://docs.docker.com/engine/api/v1.29).  Rather than manually write wrappers that fit the output docker gives, `stevedore` _generates_ an interface directly from the spefification.  Currently `stevedore` supports docker API versions 1.25 to 1.25 (defaulting to 1.29).

This approach means that the output will be type-stable - there is no inference on what to return based on what the server chooses to return.  With a given API version, the same fields will always be returned.  Some of this information is very rich, for example, for the backgrounded container above:


```r
container$inspect(reload = FALSE)
```

```
## $id
## [1] "54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235"
##
## $created
## [1] "2017-12-13T11:19:59.640605454Z"
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
## [1] 17398
##
## $state$exit_code
## [1] 0
##
## $state$error
## [1] ""
##
## $state$started_at
## [1] "2017-12-13T11:20:00.246020254Z"
##
## $state$finished_at
## [1] "0001-01-01T00:00:00Z"
##
##
## $image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $resolv_conf_path
## [1] "/var/lib/docker/containers/54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235/resolv.conf"
##
## $hostname_path
## [1] "/var/lib/docker/containers/54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235/hostname"
##
## $hosts_path
## [1] "/var/lib/docker/containers/54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235/hosts"
##
## $log_path
## [1] "/var/lib/docker/containers/54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235/54a85878bf03b7a0bccd5c8c0864272744b18c50a44f1801c13a26daff2fd235-json.log"
##
## $node
## NULL
##
## $name
## [1] "/admiring_booth"
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
## "/var/lib/docker/overlay2/9020701a4df2e96428d62c7f07fc0f8cd8137d497f3ad0a5baa91b970a80dc4d-init/diff:/var/lib/docker/overlay2/7815d9bac55ed4ceb36bd625979dfcd06330e8253b9f71c95795ce5c5b247dd0/diff:/var/lib/docker/overlay2/2b72161d0aefe066769d9334de310b36616ae170f4472a7384324d378dd82cb5/diff:/var/lib/docker/overlay2/d4903c54aa9b839529e6b24e2293abe7cbea0093a5106726c0e93754cb105591/diff"
##                                                                                                                                                                                                                                                                                                                                                                                         merged_dir
##                                                                                                                                                                                                                                                                                                 "/var/lib/docker/overlay2/9020701a4df2e96428d62c7f07fc0f8cd8137d497f3ad0a5baa91b970a80dc4d/merged"
##                                                                                                                                                                                                                                                                                                                                                                                          upper_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/9020701a4df2e96428d62c7f07fc0f8cd8137d497f3ad0a5baa91b970a80dc4d/diff"
##                                                                                                                                                                                                                                                                                                                                                                                           work_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/9020701a4df2e96428d62c7f07fc0f8cd8137d497f3ad0a5baa91b970a80dc4d/work"
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
## [1] "54a85878bf03"
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
## [1] "02:42:ac:11:00:02"
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
* endpoints that require http hijacking are not fully supported (attach, logs) but the foundations are there to support this - stdin is likely to be a major hassle though
* endpoints that require tar input and output (equivalents of `docker cp` especially) need major work
* lots of work on parameter wrangling for the more complicated endpoints (basically things that take anything more complicated than a string array are prone to failure because I've not tested them yet)
* swarm features (`nodes`, `plugins`, `secrets`, `services` and `swarm`) are not implemented - not because they'd be any harder but just because I've never used them
* authentication of registries for pull and push (not hard, just not done)
* some sort of documentation - hard given that the interface is directly pulled from the spec so we need to patch up references to json

## Licence

MIT © [Rich FitzJohn](https://github.com/richfitz).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
