<!-- -*-markdown-*- -->
# stevedore

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/richfitz/stevedore/workflows/R-CMD-check/badge.svg)](https://github.com/richfitz/stevedore/actions)
[![codecov.io](https://codecov.io/github/richfitz/stevedore/coverage.svg?branch=master)](https://app.codecov.io/github/richfitz/stevedore?branch=master)
[![](https://www.r-pkg.org/badges/version/stevedore)](https://cran.r-project.org/package=stevedore)
<!-- badges: end -->

A docker client for R



![hello world example of stevedore](https://raw.githubusercontent.com/richfitz/stevedore/master/demo/hello.gif)


## Background

**What is docker?** Docker is a platform for "containerising" applications - running them in isolation from one another, removing differences of how they are built, what they are built from, and what resources they need (disk, ports, users, etc).  It's similar conceptually to virtualisation, but much more light weight.

**Why would one want to use docker from R?** Whenever you need to control external processes from an R script or package, it might be useful to interact with this process from in containers using docker

- *package authors* might want a clean environment to test their code (similar to travis)
- *a developer using an external database* in a package or in an analysis script might use docker to create disposable copies of the database, or to create a copy isolated from their production database
- *a researcher* might use docker to do a reproducible analysis, or preserve the artefacts that were created along with a copy of the environment that created them

These are discussed further in the [applications vignette](https://richfitz.github.io/stevedore/articles/examples.html)


## Usage

The main function in the package is `docker_client`; this will construct an object with which we can talk with the docker server.


```r
docker <- stevedore::docker_client()
```

```
## Detected API version '1.43' is above max version '1.39'; downgrading
```

```r
docker
```

```
## <docker_client>
##   config: Manage docker swarm configs
##   container: Work with docker containers
##   image: Work with docker images
##   network: Work with docker networks
##   node: Manage docker swarm nodes
##   plugin: Work with docker plugins
##   secret: Manage docker swarm secrets
##   service: Work with docker services
##   swarm: Manage the docker swarm
##   task: Work with docker tasks
##   volume: Work with docker volumes
##   types: Methods for building complex docker types
##   api_version()
##   connection_info()
##   cp(src, dest)
##   df()
##   events(since = NULL, until = NULL, filters = NULL)
##   help(help_type = getOption("help_type"))
##   info()
##   login(username = NULL, password = NULL, email = NULL,
##       serveraddress = NULL)
##   ping()
##   request(verb, path, query = NULL, body = NULL, headers = NULL,
##       stream = NULL)
##   version()
```

With this you can run containers:


```r
docker$container$run("alpine:3.1", c("echo", "hello world"))
```

```
## Unable to find image 'alpine:3.1' locally
```

```
## Pulling from library/alpine 3.1
## Pulling fs layer 0f253aa151d7
## 0f253aa151d7: Downloading 23.27 kB/2.3 MB 1%
## Verifying Checksum 0f253aa151d7
## Download complete 0f253aa151d7
## 0f253aa151d7: Extracting 32.77 kB/2.3 MB 1%
## 0f253aa151d7: Extracting 2.3 MB/2.3 MB 100%
## Pull complete 0f253aa151d7
## Digest: sha256:4dfc68bc95af5c1beb5e307133ce91546874dcd0d880736b25ddbe6f483c65b4
## Status: Downloaded newer image for alpine:3.1
## O> hello world
```

```
## <docker_run_output>
##   $container:
##     <docker_container>
##       id: d4510f1195c43ce9129fc63f4cfaaae0c24332271c869b5738532d071f8d873e
##       name: dreamy_khayyam
##
##   $logs:
##     O> hello world
```

Or run containers in the background


```r
docker$container$run("bfirsh/reticulate-splines", detach = TRUE)
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL, domainname = NULL,
##       user = NULL, attach_stdin = NULL, attach_stdout = NULL,
##       attach_stderr = NULL, exposed_ports = NULL, tty = NULL,
##       open_stdin = NULL, stdin_once = NULL, env = NULL, cmd = NULL,
##       healthcheck = NULL, args_escaped = NULL, image = NULL, volumes = NULL,
##       working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##       mac_address = NULL, on_build = NULL, labels = NULL,
##       stop_signal = NULL, stop_timeout = NULL, shell = NULL)
##   cp_in(src, dest)
##   cp_out(src, dest)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE, detach_keys = NULL,
##       tty = NULL, env = NULL, privileged = NULL, user = NULL,
##       working_dir = NULL, detach = FALSE, stream = stdout())
##   exec_create(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL, working_dir = NULL)
##   export()
##   get_archive(path, dest)
##   help(help_type = getOption("help_type"))
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels(reload = TRUE)
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE, since = NULL,
##       until = NULL, timestamps = NULL, tail = NULL, stream = stdout())
##   name()
##   path_stat(path)
##   pause()
##   ports(reload = TRUE)
##   put_archive(src, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(delete_volumes = NULL, force = NULL, link = NULL)
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
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL, cpuset_mems = NULL,
##       devices = NULL, device_cgroup_rules = NULL, disk_quota = NULL,
##       kernel_memory = NULL, memory_reservation = NULL, memory_swap = NULL,
##       memory_swappiness = NULL, nano_cpus = NULL, oom_kill_disable = NULL,
##       init = NULL, pids_limit = NULL, ulimits = NULL, cpu_count = NULL,
##       cpu_percent = NULL, io_maximum_iops = NULL,
##       io_maximum_bandwidth = NULL, restart_policy = NULL)
##   wait(condition = NULL)
```

You can manage containers


```r
docker$container$list()
```

```
##                                                                 id        names
## 1 ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753 vigorous....
## 2 49e75b4d5206a0d4cafd854cc076c2b84430d3ac3632b0acf5c346871517275f        wodin
## 3 2918396637cf276204dc9026e9f456b80c494600970aed496b018a16158c3850        redis
## 4 4acc5ec7e3b293bb7d2cf510ca7630bab7558b530e6ea7b93bd11ab40ddfd9fa     odin.api
##                                                                     image
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2                                                   mrcide/wodin:mrc-4305
## 3                                                                 redis:6
## 4                                                    mrcide/odin.api:main
##                                                                  image_id
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2 sha256:765018081ad356b2fb77191dbb565f851ebf505a2b098e78440c6e290b6b1086
## 3 sha256:136fbb04568e1c1afd4cce7e7459641b89577079a179718006adb3d055aa1b0c
## 4 sha256:9c9e814d84b10fc7a3fd4332ff6a05a5580697b343fc22ba034eec21c20370fb
##                             command    created        ports size_rw
## 1             /usr/local/bin/run.sh 1688982169 characte....      NA
## 2       /wodin/docker/wodin /config 1688980021 characte....      NA
## 3 docker-entrypoint.sh redis-server 1688980006 c("0.0.0....      NA
## 4           /usr/local/bin/odin_api 1688980006 c("0.0.0....      NA
##   size_root_fs       labels   state        status host_config network_settings
## 1           NA              running   Up 1 second     default     list(bri....
## 2           NA ubuntu, .... running Up 35 minutes        host     list(hos....
## 3           NA              running Up 36 minutes     default     list(bri....
## 4           NA Carl Boe.... running Up 36 minutes     default     list(bri....
##         mounts           name
## 1 characte.... vigorous_ellis
## 2 NA, /hom....          wodin
## 3 NA, , vo....          redis
## 4 characte....       odin.api
```

```r
id <- docker$container$list(limit = 1L)$id
container <- docker$container$get(id)
container
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL, domainname = NULL,
##       user = NULL, attach_stdin = NULL, attach_stdout = NULL,
##       attach_stderr = NULL, exposed_ports = NULL, tty = NULL,
##       open_stdin = NULL, stdin_once = NULL, env = NULL, cmd = NULL,
##       healthcheck = NULL, args_escaped = NULL, image = NULL, volumes = NULL,
##       working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##       mac_address = NULL, on_build = NULL, labels = NULL,
##       stop_signal = NULL, stop_timeout = NULL, shell = NULL)
##   cp_in(src, dest)
##   cp_out(src, dest)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE, detach_keys = NULL,
##       tty = NULL, env = NULL, privileged = NULL, user = NULL,
##       working_dir = NULL, detach = FALSE, stream = stdout())
##   exec_create(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL, working_dir = NULL)
##   export()
##   get_archive(path, dest)
##   help(help_type = getOption("help_type"))
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels(reload = TRUE)
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE, since = NULL,
##       until = NULL, timestamps = NULL, tail = NULL, stream = stdout())
##   name()
##   path_stat(path)
##   pause()
##   ports(reload = TRUE)
##   put_archive(src, path, no_overwrite_dir_non_dir = NULL)
##   reload()
##   remove(delete_volumes = NULL, force = NULL, link = NULL)
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
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL, cpuset_mems = NULL,
##       devices = NULL, device_cgroup_rules = NULL, disk_quota = NULL,
##       kernel_memory = NULL, memory_reservation = NULL, memory_swap = NULL,
##       memory_swappiness = NULL, nano_cpus = NULL, oom_kill_disable = NULL,
##       init = NULL, pids_limit = NULL, ulimits = NULL, cpu_count = NULL,
##       cpu_percent = NULL, io_maximum_iops = NULL,
##       io_maximum_bandwidth = NULL, restart_policy = NULL)
##   wait(condition = NULL)
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
## O> Reticulating spline 2...
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
head(docker$image$list())
```

```
##                                                                        id
## 1 sha256:a3a43d6970d3009562b2f2ea36066c036714755c451fbbbbaa8f540847e29f56
## 2 sha256:45b749cadc502f3ac8128bbe32591b4222e53f98ceaec9731ed2e271548dfc91
## 3 sha256:765018081ad356b2fb77191dbb565f851ebf505a2b098e78440c6e290b6b1086
## 4 sha256:c2645e61efe369c9d8b67e1a56c668fb4607b8f2d84e0129233c388df8d97634
## 5 sha256:6efd27eba5c1ca89358fd4c915b14d561c1e65908d686f81fcdeade595a8d573
## 6 sha256:4f745a9460a60d6cbc502d09ce14afabe674bee90ec37edfeb1e49a02b501f81
##   parent_id    repo_tags repo_digests    created       size shared_size
## 1           vimc/ord.... vimc/ord.... 1688636365 1383974221          -1
## 2           vimc/ord.... vimc/ord.... 1688636146 1385872710          -1
## 3           mrcide/w.... mrcide/w.... 1688579104 1135143670          -1
## 4           mrcide/w.... mrcide/w.... 1688570390 1110212528          -1
## 5           vimc/ord.... vimc/ord.... 1688478269 1252578456          -1
## 6           rocker/r.... rocker/r.... 1688353223 4218933021          -1
##   virtual_size       labels containers
## 1   1383974221 Carl Boe....         -1
## 2   1385872710 Carl Boe....         -1
## 3   1135143670 ubuntu, ....         -1
## 4   1110212528 ubuntu, ....         -1
## 5   1252578456 Carl Boe....         -1
## 6   4218933021 Dirk Edd....         -1
```

Some of these functions have many arguments, but `stevedore` includes help inline:


```r
docker$container$create
```

```
## function(image, cmd = NULL, hostname = NULL, domainname = NULL, user = NULL,
##     attach_stdin = NULL, attach_stdout = NULL, attach_stderr = NULL,
##     ports = NULL, tty = NULL, open_stdin = NULL, stdin_once = NULL,
##     env = NULL, health_check = NULL, args_escaped = NULL, volumes = NULL,
##     working_dir = NULL, entrypoint = NULL, network_disabled = NULL,
##     mac_address = NULL, on_build = NULL, labels = NULL, stop_signal = NULL,
##     stop_timeout = NULL, shell = NULL, host_config = NULL, network = NULL,
##     name = NULL)
## ----------------------------------------------------------------------------
## Create a container. Similar to the cli command `docker create` or
##   `docker container create`.
## ----------------------------------------------------------------------------
##   image: The name of the image to use when creating the container
##   cmd: Command to run specified as a string or an array of strings.
##   hostname: The hostname to use for the container, as a valid RFC 1123
##         hostname.
##   domainname: The domain name to use for the container.
##   user: The user that commands are run as inside the container.
##   attach_stdin: Whether to attach to `stdin`.
##   attach_stdout: Whether to attach to `stdout`.
##   attach_stderr: Whether to attach to `stderr`.
##   ports: An object mapping ports to an empty object in the form:
##
##         `{"<port>/<tcp|udp|sctp>": {}}`
##   tty: Attach standard streams to a TTY, including `stdin` if it is not
##         closed.
##   open_stdin: Open `stdin`
##   stdin_once: Close `stdin` after one attached client disconnects
##   env: A list of environment variables to set inside the container in
##         the form `["VAR=value", ...]`. A variable without `=` is
##         removed from the environment, rather than to have an empty
##         value.
##   health_check: A test to perform to check that the container is
##         healthy. Construct with `$types$health_config()`
##   args_escaped: Command is already escaped (Windows only)
##   volumes: An object mapping mount point paths inside the container to
##         empty objects.
##   working_dir: The working directory for commands to run in.
##   entrypoint: The entry point for the container as a string or an array
##         of strings.
##
##         If the array consists of exactly one empty string (`[""]`) then
##         the entry point is reset to system default (i.e., the entry
##         point used by docker when there is no `ENTRYPOINT` instruction
##         in the `Dockerfile`).
##   network_disabled: Disable networking for the container.
##   mac_address: MAC address of the container.
##   on_build: `ONBUILD` metadata that were defined in the image's
##         `Dockerfile`.
##   labels: User-defined key/value metadata.
##   stop_signal: Signal to stop a container as a string or unsigned
##         integer.
##   stop_timeout: Timeout to stop a container in seconds.
##   shell: Shell for when `RUN`, `CMD`, and `ENTRYPOINT` uses a shell.
##   host_config: Container configuration that depends on the host we are
##         running on
##   network: This container's networking configuration.
##   name: Assign the specified name to the container. Must match
##         `/?[a-zA-Z0-9_-]+`.
```

as well as via an `help()` method on each object (e.g., `docker$help()`, `docker$container$help()`) which will display help for the API version that you are using.

## Approach

The Docker API is [versioned](https://docs.docker.com/develop/sdk/#api-version-matrix) and each version includes a [machine-readable API specification](https://docs.docker.com/engine/api/v1.29).  Rather than manually write wrappers that fit the output docker gives, `stevedore` _generates_ an interface directly from the spefification.  Currently `stevedore` supports docker API versions 1.25 to 1.39 (defaulting to 1.29).

This approach means that the output will be type-stable - there is no inference on what to return based on what the server chooses to return.  With a given API version, the same fields will always be returned.  Some of this information is very rich, for example, for the backgrounded container above:


```r
container$inspect(reload = FALSE)
```

```
## $id
## [1] "ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753"
##
## $created
## [1] "2023-07-10T09:42:49.228464401Z"
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
## [1] 1424698
##
## $state$exit_code
## [1] 0
##
## $state$error
## [1] ""
##
## $state$started_at
## [1] "2023-07-10T09:42:55.842551092Z"
##
## $state$finished_at
## [1] "0001-01-01T00:00:00Z"
##
##
## $image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $resolv_conf_path
## [1] "/var/lib/docker/containers/ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753/resolv.conf"
##
## $hostname_path
## [1] "/var/lib/docker/containers/ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753/hostname"
##
## $hosts_path
## [1] "/var/lib/docker/containers/ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753/hosts"
##
## $log_path
## [1] "/var/lib/docker/containers/ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753/ef589f8beed99294df47e495b25486e354ca75991d15aa10d0835191c373e753-json.log"
##
## $node
## NULL
##
## $name
## [1] "/vigorous_ellis"
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
## [1] "docker-default"
##
## $exec_ids
## character(0)
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
## [1] NA
##
## $host_config$kernel_memory
## [1] NA
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
## $host_config$init
## [1] NA
##
## $host_config$pids_limit
## [1] NA
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
## [1] target         source         type           read_only      consistency
## [6] bind_options   volume_options tmpfs_options
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
## [1] "private"
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
## $host_config$masked_paths
##  [1] "/proc/asound"        "/proc/acpi"          "/proc/kcore"
##  [4] "/proc/keys"          "/proc/latency_stats" "/proc/timer_list"
##  [7] "/proc/timer_stats"   "/proc/sched_debug"   "/proc/scsi"
## [10] "/sys/firmware"
##
## $host_config$readonly_paths
## [1] "/proc/bus"           "/proc/fs"            "/proc/irq"
## [4] "/proc/sys"           "/proc/sysrq-trigger"
##
##
## $graph_driver
## $graph_driver$name
## [1] "overlay2"
##
## $graph_driver$data
##                                                                                                                                                                                                                                                                                                                                                                                          lower_dir
## "/var/lib/docker/overlay2/4558f34eef2266fc830224f23cfd2b9567574d4724a3563f73929b764539ed00-init/diff:/var/lib/docker/overlay2/4a306a6be80a50ec0b4a3b5d6fba91ca6c5158fa8a09af50a09093e0766fce9d/diff:/var/lib/docker/overlay2/6533ed75a65bc4c70c9c9382a056673b6165c836bb81eece153d4f417bb4942c/diff:/var/lib/docker/overlay2/a6806a12273b00b201f6ec547f66a0e799da54c554dfab81d6dbeb439e183704/diff"
##                                                                                                                                                                                                                                                                                                                                                                                         merged_dir
##                                                                                                                                                                                                                                                                                                 "/var/lib/docker/overlay2/4558f34eef2266fc830224f23cfd2b9567574d4724a3563f73929b764539ed00/merged"
##                                                                                                                                                                                                                                                                                                                                                                                          upper_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/4558f34eef2266fc830224f23cfd2b9567574d4724a3563f73929b764539ed00/diff"
##                                                                                                                                                                                                                                                                                                                                                                                           work_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/4558f34eef2266fc830224f23cfd2b9567574d4724a3563f73929b764539ed00/work"
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
## [1] "ef589f8beed9"
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
## [1] NA
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
## $network_settings$sandbox_id
## [1] "fff145102f170f4b22dcf24aa97d7f7bc6729a2ec17ed96670baf0f2e323eada"
##
## $network_settings$hairpin_mode
## [1] FALSE
##
## $network_settings$link_local_ipv6_address
## [1] ""
##
## $network_settings$link_local_ipv6_prefix_len
## [1] 0
##
## $network_settings$ports
## list()
##
## $network_settings$sandbox_key
## [1] "/var/run/docker/netns/fff145102f17"
##
## $network_settings$secondary_ipaddresses
## [1] addr       prefix_len
## <0 rows> (or 0-length row.names)
##
## $network_settings$secondary_ipv6_addresses
## [1] addr       prefix_len
## <0 rows> (or 0-length row.names)
##
## $network_settings$endpoint_id
## [1] "7e3b4a31873e87842a1c22b41b978a141672b63fbf9685d9f3ee711c2ddfe64c"
##
## $network_settings$gateway
## [1] "172.17.0.1"
##
## $network_settings$global_ipv6_address
## [1] ""
##
## $network_settings$global_ipv6_prefix_len
## [1] 0
##
## $network_settings$ip_address
## [1] "172.17.0.4"
##
## $network_settings$ip_prefix_len
## [1] 16
##
## $network_settings$ipv6_gateway
## [1] ""
##
## $network_settings$mac_address
## [1] "02:42:ac:11:00:04"
##
## $network_settings$networks
## $network_settings$networks$bridge
## $network_settings$networks$bridge$ipam_config
## NULL
##
## $network_settings$networks$bridge$links
## character(0)
##
## $network_settings$networks$bridge$aliases
## character(0)
##
## $network_settings$networks$bridge$network_id
## [1] "8c42252e436fe597c5d512ca98aad98ef308891adc5366c81caa4f1ce0c5b936"
##
## $network_settings$networks$bridge$endpoint_id
## [1] "7e3b4a31873e87842a1c22b41b978a141672b63fbf9685d9f3ee711c2ddfe64c"
##
## $network_settings$networks$bridge$gateway
## [1] "172.17.0.1"
##
## $network_settings$networks$bridge$ip_address
## [1] "172.17.0.4"
##
## $network_settings$networks$bridge$ip_prefix_len
## [1] 16
##
## $network_settings$networks$bridge$ipv6_gateway
## [1] ""
##
## $network_settings$networks$bridge$global_ipv6_address
## [1] ""
##
## $network_settings$networks$bridge$global_ipv6_prefix_len
## [1] 0
##
## $network_settings$networks$bridge$mac_address
## [1] "02:42:ac:11:00:04"
##
## $network_settings$networks$bridge$driver_opts
## NULL
```

## Windows support

Windows support for docker through this package is currently incomplete.  The [appveyor build](https://ci.appveyor.com/project/richfitz/stevedore) tests only the parts of the package that don't actually call docker due to difficult-to-debug issues with container compatibility on that platform.

### Current situation

The support for windows is not as comprehensive as for other platforms (but I'm not sure how common using docker is on windows yet).  The reason for this is that [`curl`](https://cran.r-project.org/package=curl) (and the underlying `libcurl` library) do not support communicating over "named pipes" which is how docker works on windows 10.

The package includes a small wrapper around the python sdk's docker support.  On the R side this requires the `reticulate` package.  It also requires that you have a python installation that includes both the `docker` package and `pypiwin32` - once python is installed you can add these packages to python with

```
pip install docker pypiwin32
```

(or `pip3` instead of `pip` to use python3).

You can check that everything is configured by running

```r
stevedore:::httppipe_available(verbose = TRUE)
```

which will return `TRUE` if everything is OK, and otherwise print some information about errors loading the package.  In the case of error consult the reticulate documentation (`vignette("versions", package = "reticulate")` will be especially useful).  Improvements to installation documentation and process are welcome!

### Limitations

The primary limitation of the `httppipe` interface is that streaming connections are not supported.  This affects the following methods

* container logs with `follow = TRUE`: completely unsupported
* container run - works completely with `detach = TRUE`, and with `detach = FALSE` works but prints output only at the end (not streaming output)
* image build - works but information printed only at end of build rather than streaming
* image pull - works but information printed only at end of pull
* exec start - works but information printed only at end of execution

### Going forward

The support in this package is a duplicate of the micropackage [`httppipe`](https://github.com/richfitz/httppipe/).  This implements the minimum functionality with windows named pipes to support docker.  I would **love** for someone to help port this to a python-free package.  This probably requires a bit of C/C++ and knowledge of the win32 API.

## Development and testing

See the [development guide](https://github.com/richfitz/stevedore/blob/master/development.md) if you want to get started developing `stevedore` - it provides pointers to the core objects.

## Package limitations

Endpoints that require "http hijacking" are not fully supported (primarily `attach`) but the foundations are there to support this - stdin is likely to be a major hassle though and I'm not sure if it's possible from within R's REPL.

## Installation

`stevedore` can be installed from CRAN using

```r
install.packages("stevedore")
```

On windows you will also need `reticulate`

```r
install.packages("reticulate")
```

You will also need a python installation and the `docker` and `pypiwin32` packages, which can be installed with pip (see [above](#windows-support)).

Once installed, find out if everything is set up to use docker by running


```r
stevedore::docker_available()
```

```
## Detected API version '1.43' is above max version '1.39'; downgrading
```

```
## [1] TRUE
```

To install the development version from GitHub, you can use `remotes`:

```r
remotes::install_github("richfitz/stevedore", upgrade = FALSE)
```

## Licence

MIT © [Rich FitzJohn](https://github.com/richfitz).

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/richfitz/stevedore/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.
