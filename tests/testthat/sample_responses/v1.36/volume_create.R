## version: 1.36
## method: post
## path: /volumes/create
## code: 201
list(name = "tardis",
     driver = "custom",
     mountpoint = "/var/lib/docker/volumes/tardis",
     created_at = "2016-06-07T20:31:11.853781916Z",
     status = list(hello = "world"),
     labels = c(
       "com.example.some-label" = "some-value",
       "com.example.some-other-label" = "some-other-value"),
     scope = "local",
     options = NULL,
     usage_data = NULL)
