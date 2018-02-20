<!-- -*-markdown-*- -->
# stevedore

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/stevedore.svg?branch=master)](https://travis-ci.org/richfitz/stevedore)
[![codecov.io](https://codecov.io/github/richfitz/stevedore/coverage.svg?branch=master)](https://codecov.io/github/richfitz/stevedore?branch=master)

A docker client for R



![hello world example of stevedore](https://raw.githubusercontent.com/richfitz/stevedore/master/demo/hello.gif)

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
## O> hello world
```

```
## <docker_run_output>
##    $container:
##      <docker_container>
##        id: 4412c6815c7042da263a0f53bf66373c14492df7495fd10dcd714319553eaef4
##        name: eager_neumann
##
##    $logs:
##      O> hello world
```

Or run containers in the background


```r
docker$containers$run("bfirsh/reticulate-splines", detach = TRUE)
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL,
##       domainname = NULL, user = NULL, attach_stdin = NULL,
##       attach_stdout = NULL, attach_stderr = NULL, exposed_ports = NULL,
##       tty = NULL, open_stdin = NULL, stdin_once = NULL,
##       env = NULL, cmd = NULL, healthcheck = NULL, args_escaped = NULL,
##       image = NULL, volumes = NULL, working_dir = NULL,
##       entrypoint = NULL, network_disabled = NULL, mac_address = NULL,
##       on_build = NULL, labels = NULL, stop_signal = NULL,
##       stop_timeout = NULL, shell = NULL)
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
##   labels()
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE,
##       since = NULL, timestamps = NULL, tail = NULL, stream = stdout())
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
## 1 9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84
## 2 1a7aa5c125afed0556144daad4df35d6b9ecdd7650f2311498a687a28e00490e
## 3 e48e694af54b98c4a3a977d7b1682e6127cf08f19f821f8980746d45aefef557
##          names
## 1 nervous_....
## 2 stevedor....
## 3 vibrant_....
##                                                                     image
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2                                                        richfitz/iterate
## 3 sha256:3f8a4339aadda5897b744682f5f774dc69991a81af8d715d37a616bb4c99edf5
##                                                                  image_id
## 1 sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9
## 2 sha256:703226f2a06f2f48bb36120bc6aa85dcb35693ec0d68731fea1d33c06b5f24a7
## 3 sha256:3f8a4339aadda5897b744682f5f774dc69991a81af8d715d37a616bb4c99edf5
##                          command    created        ports size_rw
## 1          /usr/local/bin/run.sh 1519111926 characte....      NA
## 2 /usr/local/bin/iterate 100 100 1519111047 characte....      NA
## 3         nginx -g 'daemon off;' 1519110425 0.0.0.0,....      NA
##   size_root_fs       labels   state                status host_config
## 1           NA              running Up Less than a second     default
## 2           NA        0.0.1 running         Up 14 minutes     default
## 3           NA NGINX Do.... running         Up 25 minutes     default
##   network_settings       mounts                 name
## 1     list(bri.... characte....      nervous_babbage
## 2     list(bri.... characte.... stevedore_hdaprofvcb
## 3     list(bri.... characte....   vibrant_tereshkova
```

```r
id <- docker$containers$list(limit = 1L)$id
container <- docker$containers$get(id)
container
```

```
## <docker_container>
##   commit(repo = NULL, tag = NULL, author = NULL, changes = NULL,
##       comment = NULL, pause = NULL, hostname = NULL,
##       domainname = NULL, user = NULL, attach_stdin = NULL,
##       attach_stdout = NULL, attach_stderr = NULL, exposed_ports = NULL,
##       tty = NULL, open_stdin = NULL, stdin_once = NULL,
##       env = NULL, cmd = NULL, healthcheck = NULL, args_escaped = NULL,
##       image = NULL, volumes = NULL, working_dir = NULL,
##       entrypoint = NULL, network_disabled = NULL, mac_address = NULL,
##       on_build = NULL, labels = NULL, stop_signal = NULL,
##       stop_timeout = NULL, shell = NULL)
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
##   labels()
##   logs(follow = NULL, stdout = TRUE, stderr = TRUE,
##       since = NULL, timestamps = NULL, tail = NULL, stream = stdout())
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
## 1 sha256:70c59324ba80a90c2bf44a944dba09af447036b0596b355750b30593e6d977cb
## 2 sha256:a3481e4b3759ee3160aa3f781d336ba25cdb57b261b0c34501388873ba055eec
## 3 sha256:39185b7b6a5deb8ac58ae805c52fcb4c8a6ba1aa5ea6259c6f5b3a2e214f3d7c
## 4 sha256:9f09a7f472995bcd18b257f1db079f836eabdfa972804477288519e23487f198
## 5 sha256:30cbbf5b7745ae037b6dabbd3a56a143952927d22e8a7c69a401a74cbaacc254
## 6 sha256:296fa2bbf65dfc7fbec5e772f007f995e3573affe3351590492f65ff529e97dd
##                                                                 parent_id
## 1 sha256:5a5f4784896c1e4d432e3ad2298fa1e9fbd17cfb267d65dc482f132f804f8f61
## 2 sha256:b2f9e699ed64353851505a6c020b90a8dad68d1ed13f0766def022d81a5ed684
## 3 sha256:46b78da174fff75afcb7f4f2c34a51c72ff912d29a18e24569a724b71786d156
## 4 sha256:72079bbd57d46a2ec45180762e779e6e3f952a006bb5ce767016f0b80725d8ff
## 5 sha256:34bca518f0a52f947ed50524df1bc4fc64ccb191f72d52fb2c11804338a4c2f2
## 6 sha256:90a51b9d763e494aa7b19295b6e122b6ebbce431d4eccd0173037a80070c481f
##      repo_tags repo_digests    created    size shared_size virtual_size
## 1 richfitz....              1519111816 4148087          -1      4148087
## 2 <none>:<.... <none>@<.... 1519111814 4148087          -1      4148087
## 3 <none>:<.... <none>@<.... 1519111813 4148087          -1      4148087
## 4 <none>:<.... <none>@<.... 1519111812 4148087          -1      4148087
## 5 <none>:<.... <none>@<.... 1519111811 4148087          -1      4148087
## 6 <none>:<.... <none>@<.... 1519111809 4148087          -1      4148087
##   labels containers
## 1  0.0.1         -1
## 2  0.0.1         -1
## 3  0.0.1         -1
## 4  0.0.1         -1
## 5  0.0.1         -1
## 6  0.0.1         -1
```

## Approach

Docker publishes a [machine-readable API specification](https://docs.docker.com/engine/api/v1.29).  Rather than manually write wrappers that fit the output docker gives, `stevedore` _generates_ an interface directly from the spefification.  Currently `stevedore` supports docker API versions 1.25 to 1.36 (defaulting to 1.29).

This approach means that the output will be type-stable - there is no inference on what to return based on what the server chooses to return.  With a given API version, the same fields will always be returned.  Some of this information is very rich, for example, for the backgrounded container above:


```r
container$inspect(reload = FALSE)
```

```
## $id
## [1] "9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84"
##
## $created
## [1] "2018-02-20T07:32:06.4341075Z"
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
## [1] 24254
##
## $state$exit_code
## [1] 0
##
## $state$error
## [1] ""
##
## $state$started_at
## [1] "2018-02-20T07:32:06.9160252Z"
##
## $state$finished_at
## [1] "0001-01-01T00:00:00Z"
##
##
## $image
## [1] "sha256:b1666055931f332541bda7c425e624764de96c85177a61a0b49238a42b80b7f9"
##
## $resolv_conf_path
## [1] "/var/lib/docker/containers/9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84/resolv.conf"
##
## $hostname_path
## [1] "/var/lib/docker/containers/9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84/hostname"
##
## $hosts_path
## [1] "/var/lib/docker/containers/9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84/hosts"
##
## $log_path
## [1] "/var/lib/docker/containers/9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84/9723e338954de7a5f680178871d581e1a2801659d00dcf71920580fb97330b84-json.log"
##
## $node
## NULL
##
## $name
## [1] "/nervous_babbage"
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
## "/var/lib/docker/overlay2/28b26245843f674bf232d52a21c606acfabc5d1bb9d227b3aae834e2ad7ca381-init/diff:/var/lib/docker/overlay2/7815d9bac55ed4ceb36bd625979dfcd06330e8253b9f71c95795ce5c5b247dd0/diff:/var/lib/docker/overlay2/2b72161d0aefe066769d9334de310b36616ae170f4472a7384324d378dd82cb5/diff:/var/lib/docker/overlay2/d4903c54aa9b839529e6b24e2293abe7cbea0093a5106726c0e93754cb105591/diff"
##                                                                                                                                                                                                                                                                                                                                                                                         merged_dir
##                                                                                                                                                                                                                                                                                                 "/var/lib/docker/overlay2/28b26245843f674bf232d52a21c606acfabc5d1bb9d227b3aae834e2ad7ca381/merged"
##                                                                                                                                                                                                                                                                                                                                                                                          upper_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/28b26245843f674bf232d52a21c606acfabc5d1bb9d227b3aae834e2ad7ca381/diff"
##                                                                                                                                                                                                                                                                                                                                                                                           work_dir
##                                                                                                                                                                                                                                                                                                   "/var/lib/docker/overlay2/28b26245843f674bf232d52a21c606acfabc5d1bb9d227b3aae834e2ad7ca381/work"
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
## [1] "9723e338954d"
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
## [1] "02:42:ac:11:00:04"
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
* `.dockerignore` files are not currently honoured
* lots of work on parameter wrangling for the more complicated endpoints (basically things that take anything more complicated than a string array are prone to failure because I've not tested them yet)
* swarm features (`nodes`, `plugins`, `secrets`, `services` and `swarm`) are not implemented - not because they'd be any harder but just because I've never used them
* authentication of registries for pull and push (not hard, just not done)
* some sort of documentation - hard given that the interface is directly pulled from the spec so we need to patch up references to json

## Development and testing

**WARNING**: package is in early development. If you run the test suite it will do all sorts of things to your containers/images/volumes/networks.  Do not run it unless you'd be happy running `docker system prune -f --volumes`.  The examples are more polite.

At present, the tests do leave a couple of orphan containers and volumes on exit.  If tests fail, many more containers will be left as teardown is not always automatic.  Some tests are of the `prune` endpoints, others pull images from the internet.

## Installation

Currently not on CRAN, but can be installed from github with devtools

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
