## version: 1.36
## method: get
## path: /volumes
## code: 200
list(volumes = data.frame(
       name = "tardis",
       driver = "local",
       mountpoint = "/var/lib/docker/volumes/tardis",
       created_at = "2017-07-19T12:00:26Z",
       ## TODO: status here should be list(character())
       status = I(list(list())),
       labels = I(list(c("com.example.some-label" = "some-value",
                         "com.example.some-other-label" =
                           "some-other-value"))),
       scope = "local",
       options = I(list(c(device = "tmpfs",
                          o = "size=100m,uid=1000",
                          type = "tmpfs"))),
       usage_data = I(list(list(size = NA_integer_,
                                ref_count = NA_integer_))),
       stringsAsFactors = FALSE),
     warnings = character())
