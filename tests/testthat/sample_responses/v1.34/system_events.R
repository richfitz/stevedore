## version: 1.34
## method: get
## path: /events
## code: 200
actor <- list(
  id = "ede54ee1afda366ab42f824e8a5ffd195155d853ceaec74a927f249ea270c743",
  attributes = c(
    "com.example.some-label" = "some-label-value",
    image = "alpine",
    name = "my-container"))
list(
  type = "container",
  action = "create",
  actor = actor,
  time = 1461943101L,
  time_nano = NA_integer_)
