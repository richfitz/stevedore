<!-- -*-markdown-*- -->
# stevedore

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/stevedore.svg?branch=master)](https://travis-ci.org/richfitz/stevedore)
[![codecov.io](https://codecov.io/github/richfitz/stevedore/coverage.svg?branch=master)](https://codecov.io/github/richfitz/stevedore?branch=master)

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
docker
```

```
## <docker_client>
##   configs: docker_config_collection
##   containers: docker_container_collection
##   images: docker_image_collection
##   networks: docker_network_collection
##   nodes: docker_swarm_collection
##   secrets: docker_secret_collection
##   services: docker_service_collection
##   swarm: docker_swarm_collection
##   tasks: docker_task_collection
##   volumes: docker_volume_collection
##   types: docker_types
##   api_version()
##   df()
##   events(since = NULL, until = NULL, filters = NULL)
##   help(help_type = getOption("help_type"))
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
## 61aa778aed31: Extracting 1.25 MB/2.3 MB 54%
## 61aa778aed31: Extracting 2.3 MB/2.3 MB 100%
## Pull complete 61aa778aed31
## Digest: sha256:10de714727daa45047abdfb81c98dbf45e1cad3b590b5043d0da139bfeacebe5
## Status: Downloaded newer image for alpine:3.1
## O> hello world
```

```
## <docker_run_output>
##   $container:
##     <docker_container>
##       id: 5c1721d8554be6a19c1d0eb9c6af6b310d4b8ae2220857ad072fe61cf0c8d8a2
##       name: awesome_goldwasser
##
##   $logs:
##     O> hello world
```

Or run containers in the background


```r
docker$containers$run("bfirsh/reticulate-splines", detach = TRUE)
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL, domainname = NULL,
##       user = NULL, attach_stdin = NULL, attach_stdout = NULL,
##       attach_stderr = NULL, exposed_ports = NULL, tty = NULL,
##       open_stdin = NULL, stdin_once = NULL, env = NULL, cmd = NULL,
##       healthcheck = NULL, args_escaped = NULL, image = NULL,
##       volumes = NULL, working_dir = NULL, entrypoint = NULL,
##       network_disabled = NULL, mac_address = NULL, on_build = NULL,
##       labels = NULL, stop_signal = NULL, stop_timeout = NULL,
##       shell = NULL)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL)
##   export()
##   get_archive(path, dest)
##   help(help_type = getOption("help_type"))
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels(reload = TRUE)
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE, since = NULL,
##       timestamps = NULL, tail = NULL, stream = stdout())
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
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL,
##       cpuset_mems = NULL, devices = NULL, device_cgroup_rules = NULL,
##       disk_quota = NULL, kernel_memory = NULL,
##       memory_reservation = NULL, memory_swap = NULL,
##       memory_swappiness = NULL, nano_cpus = NULL,
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
## 1 ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a
##          names
## 1 vigilant....
##                                                                     image
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
##                                                                  image_id
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
##                 command    created        ports size_rw size_root_fs
## 1 /usr/local/bin/run.sh 1522162863 characte....      NA           NA
##   labels   state                status host_config network_settings
## 1        running Up Less than a second     default     list(bri....
##         mounts          name
## 1 characte.... vigilant_benz
```

```r
id <- docker$containers$list(limit = 1L)$id
container <- docker$containers$get(id)
container
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL, domainname = NULL,
##       user = NULL, attach_stdin = NULL, attach_stdout = NULL,
##       attach_stderr = NULL, exposed_ports = NULL, tty = NULL,
##       open_stdin = NULL, stdin_once = NULL, env = NULL, cmd = NULL,
##       healthcheck = NULL, args_escaped = NULL, image = NULL,
##       volumes = NULL, working_dir = NULL, entrypoint = NULL,
##       network_disabled = NULL, mac_address = NULL, on_build = NULL,
##       labels = NULL, stop_signal = NULL, stop_timeout = NULL,
##       shell = NULL)
##   diff()
##   exec(cmd, stdin = NULL, stdout = TRUE, stderr = TRUE,
##       detach_keys = NULL, tty = NULL, env = NULL, privileged = NULL,
##       user = NULL)
##   export()
##   get_archive(path, dest)
##   help(help_type = getOption("help_type"))
##   id()
##   image()
##   inspect(reload = TRUE)
##   kill(signal = NULL)
##   labels(reload = TRUE)
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE, since = NULL,
##       timestamps = NULL, tail = NULL, stream = stdout())
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
##       cpu_realtime_runtime = NULL, cpuset_cpus = NULL,
##       cpuset_mems = NULL, devices = NULL, device_cgroup_rules = NULL,
##       disk_quota = NULL, kernel_memory = NULL,
##       memory_reservation = NULL, memory_swap = NULL,
##       memory_swappiness = NULL, nano_cpus = NULL,
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
## 1 sha256:12fcbaa1b768e020e4bda482568286a9e527fd9850fc6f8e5df3aa2c3efcccfa
## 2 sha256:29f79adfac599c10a4608304f8cf93597ad208a1299591e1f23745f491dbccb7
## 3 sha256:ddb138ffcfd5c98182123a72d871350ef408e906b03d9cc947d9e3d28d5126e4
## 4 sha256:e06220dbd9c0af0b20951504b14cbf5ce106ddd27d1d251d63e51a96f9688f44
## 5 sha256:265fea0fa5c3fff4151df5136829b8b475e3ff63da70941358f13dcb8739fb8b
## 6 sha256:6912fa3f863789cb1014ad4da71c82857e5918188cddb5261a8eb9e579497ce2
##                                                                 parent_id
## 1 sha256:6c89f016add6fc97c335c70fe51b47fee95633da31edf44ce1654cff5bfa38ff
## 2
## 3
## 4
## 5
## 6
##      repo_tags repo_digests    created      size shared_size virtual_size
## 1 richfitz....              1522160216 855293381          -1    855293381
## 2 richfitz.... richfitz.... 1521560490 759007139          -1    759007139
## 3 docker.m.... docker.m.... 1521543255 263775213          -1    263775213
## 4 docker.m.... docker.m.... 1521214167 312997301          -1    312997301
## 5 docker.m.... docker.m.... 1521209642 312997187          -1    312997187
## 6 docker.m.... docker.m.... 1521209220 100448405          -1    100448405
##   labels containers
## 1                -1
## 2                -1
## 3                -1
## 4                -1
## 5                -1
## 6                -1
```

Some of these functions have many arguments, but `stevedore` includes help inline:


```r
docker$containers$create
```

```
## function(image, cmd = NULL, hostname = NULL, domainname = NULL,
##     user = NULL, attach_stdin = NULL, attach_stdout = NULL,
##     attach_stderr = NULL, ports = NULL, tty = NULL, open_stdin = NULL,
##     stdin_once = NULL, env = NULL, health_check = NULL,
##     args_escaped = NULL, volumes = NULL, working_dir = NULL,
##     entrypoint = NULL, network_disabled = NULL, mac_address = NULL,
##     on_build = NULL, labels = NULL, stop_signal = NULL,
##     stop_timeout = NULL, shell = NULL, host_config = NULL,
##     network = NULL, name = NULL)
## ----------------------------------------------------------------------
## Create a container.  Similar to the cli command `docker create` or
##   `docker container create`.
## ----------------------------------------------------------------------
##   image: The name of the image to use when creating the container
##   cmd: Command to run specified as a string or an array of
##         strings.
##   hostname: The hostname to use for the container, as a valid RFC
##         1123 hostname.
##   domainname: The domain name to use for the container.
##   user: The user that commands are run as inside the container.
##   attach_stdin: Whether to attach to `stdin`.
##   attach_stdout: Whether to attach to `stdout`.
##   attach_stderr: Whether to attach to `stderr`.
##   ports: A character vector of port mappings between the container
##         and host, in (1) the form `<host>:<container>` (e.g.,
##         `10080:80` to map the container's port 80 to the host's
##         port 10080), (2) the form `<port>` as shorthand for
##         `<port>:<port>`, or (3) a single logical value `TRUE`
##         indicating to map all container ports to random available
##         ports on the host.  You can use the `$ports()` method in
##         the `docker_container` object to query the port mapping of
##         a running container.
##   tty: Attach standard streams to a TTY, including `stdin` if it
##         is not closed.
##   open_stdin: Open `stdin`
##   stdin_once: Close `stdin` after one attached client disconnects
##   env: A list of environment variables to set inside the container
##         in the form `["VAR=value", ...]`. A variable without `=`
##         is removed from the environment, rather than to have an
##         empty value.
##   health_check: A test to perform to check that the container is
##         healthy. Construct with `$types$health_config()`
##   args_escaped: Command is already escaped (Windows only)
##   volumes: A character vector of mappings of mount points on the
##         host (or in volumes) to paths on the container.  Each
##         element must be of the form
##         `<path_host>:<path_container>`, possibly followed by `:ro`
##         for read-only mappings (i.e., the same syntax as the
##         docker command line client).
##         `docker_volume` objects have a `$map` method to help with
##         generating these paths for volume mappings.
##   working_dir: The working directory for commands to run in.
##   entrypoint: The entry point for the container as a string or an
##         array of strings.
##
##         If the array consists of exactly one empty string (`[""]`)
##         then the entry point is reset to system default (i.e., the
##         entry point used by docker when there is no `ENTRYPOINT`
##         instruction in the `Dockerfile`).
##   network_disabled: Disable networking for the container.
##   mac_address: MAC address of the container.
##   on_build: `ONBUILD` metadata that were defined in the image's
##         `Dockerfile`.
##   labels: User-defined key/value metadata.
##   stop_signal: Signal to stop a container as a string or unsigned
##         integer.
##   stop_timeout: Timeout to stop a container in seconds.
##   shell: Shell for when `RUN`, `CMD`, and `ENTRYPOINT` uses a
##         shell.
##   host_config: Container configuration that depends on the host we
##         are running on
##   network: This container's networking configuration.
##   name: Assign the specified name to the container. Must match
##         `/?[a-zA-Z0-9_-]+`.
```

as well as via an `help()` method on each object (e.g., `docker$help()`, `docker$containers$help()`) which will display help for the API version that you are using.

## Approach

Docker publishes a [machine-readable API specification](https://docs.docker.com/engine/api/v1.29).  Rather than manually write wrappers that fit the output docker gives, `stevedore` _generates_ an interface directly from the spefification.  Currently `stevedore` supports docker API versions 1.25 to 1.36 (defaulting to 1.29).

This approach means that the output will be type-stable - there is no inference on what to return based on what the server chooses to return.  With a given API version, the same fields will always be returned.  Some of this information is very rich, for example, for the backgrounded container above:


```r
container$inspect(reload = FALSE)
```

```
## $id
## [1] "ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a"
##
## $created
## [1] "2018-03-27T15:01:03.733952395Z"
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
## [1] 3994
##
## $state$exit_code
## [1] 0
##
## $state$error
## [1] ""
##
## $state$started_at
## [1] "2018-03-27T15:01:04.452272327Z"
##
## $state$finished_at
## [1] "0001-01-01T00:00:00Z"
##
##
## $image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $resolv_conf_path
## [1] "/mnt/storage/docker/var-lib-docker/containers/ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a/resolv.conf"
##
## $hostname_path
## [1] "/mnt/storage/docker/var-lib-docker/containers/ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a/hostname"
##
## $hosts_path
## [1] "/mnt/storage/docker/var-lib-docker/containers/ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a/hosts"
##
## $log_path
## [1] "/mnt/storage/docker/var-lib-docker/containers/ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a/ca29748dfc48d057b895ac84c43a51ebb589a23fe3f21c588ffee87d7f92123a-json.log"
##
## $node
## NULL
##
## $name
## [1] "/vigilant_benz"
##
## $restart_count
## [1] 0
##
## $driver
## [1] "aufs"
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
## [1] "aufs"
##
## $graph_driver$data
## NULL
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
## [1] "ca29748dfc48"
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


## Windows support

The support for windows is not as comprehensive as for other platforms (but I'm not sure how common using docker is on windows yet).  The reason for this is that [`curl`](https://cran.r-project.org/package=curl) (and the underlying `libcurl` library) do not support communicating over "named pipes" which is how docker works on windows.  There is partial support for this in the package using the package [`httppipe`](http://github.com/richfitz/httppipe).

### Installation

You will need a python installation (both python2 and python3 should work), along with [`reticulate`](https://cran.r-project.org/package=reticulate).  Whatever python you use needs to be able to find the python packages `requests`, `six` and `pipywin32`.  You can test if everything is working by running

```
httppipe::httppipe_available(verbose = TRUE)
```

which will return `TRUE` if everything is OK, and otherwise print some information about errors loading the package.  In the case of error consult the reticulate documentation (`vignette("versions", package = "reticulate")` will be especially useful).  Improvements to installation documentation and process are welcome!

### Limitations

The primary limitation of the `httppipe` interface is that streaming connections are not supported.  This affects the following methods

* container logs with `follow = TRUE`: completely unsupported
* container run - works completely with `detach = TRUE`, and with `detach = FALSE` works but prints output only at the end (not streaming output)
* image build - works but information printed only at end of build rather than streaming
* image pull - works but information printed only at end of pull
* exec start - works but information printed only at end of execution


## Roadmap

There is still a lot of work to do here:

* windows support needs work (see above for details)
* unix non-socket (tcp) access, including TLS.  Setting it up is not too bad but testing it securely is a pain.
* endpoints that require http hijacking are not fully supported (i.e., attach) but the foundations are there to support this - stdin is likely to be a major hassle though and I'm not sure if it's possible from within R's REPL.  The websockets approach might be better but stands very little chance of working on windows.
* endpoints that require tar input and output (equivalents of `docker cp` especially) need major work
* lots of work on parameter wrangling for the more complicated endpoints (basically things that take anything more complicated than a string array are prone to failure because I've not tested them yet)
* swarm features (`nodes`, `plugins`, `secrets`, `services` and `swarm`) are not implemented - not because they'd be any harder but just because I've never used them


## Development and testing

See the [development guide](development.md) if you want to get started developing `stevedore` - it provides pointers to the core objects.


## Installation

Currently, `stevedore` is not on CRAN, but can be installed directly from GitHub using devtools

```r
devtools::install_github("richfitz/stevedore", upgrade_dependencies = FALSE)
```

On windows you will also need

```r
devtools::install_github("richfitz/httppipe", upgrade_dependencies = FALSE)
```

(see [above](#windows-support)).

Once installed, find out if everything is set up to use docker by running


```r
stevedore::docker_available()
```

```
## [1] TRUE
```


## Licence

MIT © [Rich FitzJohn](https://github.com/richfitz).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
