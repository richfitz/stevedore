## version: 1.37
## method: post
## path: /volumes/create
## code: 201
## response: {"Name":"tardis","Driver":"custom","Mountpoint":"/var/lib/docker/volumes/tardis","Status":{"hello":"world"},"Labels":{"com.example.some-label":"some-value","com.example.some-other-label":"some-other-value"},"Scope":"local","CreatedAt":"2016-06-07T20:31:11.853781916Z"}
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
