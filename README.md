# stevedore

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/stevedore.svg?branch=master)](https://travis-ci.org/richfitz/stevedore)
[![codecov.io](https://codecov.io/github/richfitz/stevedore/coverage.svg?branch=master)](https://codecov.io/github/richfitz/stevedore?branch=master)

A docker client for R

**WARNING**: package is in early development. If you run the test suite it may do all sorts of things to your containers/images/volumes/networks.  Do not run it unless you'd be happy running `docker system prune -f`

## Approach

Docker provides _very_ rich information about running containers - much of the work of this package is converting this information back into R's native structures in a way that is both useful and predictable.  For example, the API equivalent of `docker ps` (`GET /containers/json`) returns a json array of dicts - each element of has an expected set of keys.

```json
[{"Id":"8dfafdbc3a40","Names":"/boring_feynman","Image":"ubuntu:latest","ImageID":"d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82","Command":"echo 1","Created":1367854155,"State":"Exited","Status":"Exit 0","Ports":[{"PrivatePort":2222,"PublicPort":3333,"Type":"tcp"}],"Labels":{"com.example.vendor":"Acme","com.example.license":"GPL","com.example.version":"1.0"},"SizeRw":12288,"SizeRootFs":0,"HostConfig":{"NetworkMode":"default"},"NetworkSettings":{"Networks":{"bridge":{"NetworkID":"7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812","EndpointID":"2cdc4edb1ded3631c81f57966563e5c8525b81121bb3706a9a9a3ae102711f3f","Gateway":"172.17.0.1","IPAddress":"172.17.0.2","IPPrefixLen":16,"IPv6Gateway":"","GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":"02:42:ac:11:00:02"}}},"Mounts":[{"Name":"fac362...80535","Source":"/data","Destination":"/data","Driver":"local","Mode":"ro,Z","RW":false,"Propagation":""}]},...]
```

This would be nice to render as a `data.frame` in R (each element of the array becoming a row in the data.frame).

```
            id        names         image
1 8dfafdbc3a40 /boring_.... ubuntu:latest
2 9cd87474be90    /coolName ubuntu:latest
3 3176a2479c92  /sleepy_dog ubuntu:latest
4 4cb07b47f9fb /running_cat ubuntu:latest
                                                          image_id
1 d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82
2 d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82
3 d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82
4 d74508fb6632491cea586a1fd7d748dfc5274cd6fdfedee309ecdcbc2bf5cb82
                                 command    created        ports size_rw
1                                 echo 1 1367854155 NA, 2222....   12288
2                            echo 222222 1367854155 characte....   12288
3                  echo 3333333333333333 1367854154 characte....   12288
4 echo 444444444444444444444444444444444 1367854152 characte....   12288
  size_root_fs       labels  state status host_config network_settings
1            0 Acme, GP.... Exited Exit 0     default     list(bri....
2            0              Exited Exit 0     default     list(bri....
3            0              Exited Exit 0     default     list(bri....
4            0              Exited Exit 0     default     list(bri....
        mounts
1 NA, /dat....
2 characte....
3 characte....
4 characte....
```

Some of these are "list columns", such as `ports` with the first element itself being a `data.frame`

```
   i_p private_port public_port type
1 <NA>         2222        3333  tcp
```

(it is a list of `data.frame`s because each container might have multiple ports.  Other objects are not naturally representable as `data.frame`s such as `network_settings`; the first element of this looks like

```
$networks
$networks$bridge
$networks$bridge$i_pamconfig
NULL

$networks$bridge$links
[1] NA

$networks$bridge$aliases
[1] NA

$networks$bridge$network_id
[1] "7ea29fc1412292a2d7bba362f9253545fecdfa8ce9a6e37dd10ba8bee7129812"

$networks$bridge$endpoint_id
[1] "2cdc4edb1ded3631c81f57966563e5c8525b81121bb3706a9a9a3ae102711f3f"

$networks$bridge$gateway
[1] "172.17.0.1"

$networks$bridge$i_paddress
[1] "172.17.0.2"

$networks$bridge$i_pprefix_len
[1] 16

$networks$bridge$i_pv6_gateway
[1] ""

$networks$bridge$global_ipv6_address
[1] ""

$networks$bridge$global_ipv6_prefix_len
[1] 0

$networks$bridge$mac_address
[1] "02:42:ac:11:00:02"
```

These translations are all based off of the docker spec for the requested docker API version - there is no inference based on the return type.  This means that while the functions return complicated output, it should be of a reliable format.

As a really simple case of why you'd want this; think about what `docker ps` returns when there are no running containers - a set of headers, like a `data.frame` with zero rows; so you have the structure of the data even though there is no actual information.

* We read the yaml spec to determine the mapping for the raw API functions.  Argument recievers and handlers are generated from this (currently called `docker_client_data`).
* We create an actual http client which takes care of the communication with the server (`R6_http_client`).
* From this we create a list of endpoints (endpoint data plus the http client) (`client_endpoints`) - this provides raw access to the docker API but no logic around that.  This is something that we will probably coordinate exporting.
* The actual `docker_client` object will use this

## Licence

MIT © [Rich FitzJohn](https://github.com/richfitz).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
