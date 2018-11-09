## version: 1.39
## method: get
## path: /images/json
## code: 200
## response: [{"Id":"sha256:e216a057b1cb1efc11f8a268f37ef62083e70b1b38323ba252e25ac88904a7e8","ParentId":"","RepoTags":["ubuntu:12.04","ubuntu:precise"],"RepoDigests":"ubuntu@sha256:992069aee4016783df6345315302fa59681aae51a8eeb2f889dea59290f21787","Created":1474925151,"Size":103579269,"VirtualSize":103579269,"SharedSize":0,"Labels":{},"Containers":2},{"Id":"sha256:3e314f95dcace0f5e4fd37b10862fe8398e3c60ed36600bc0ca5fda78b087175","ParentId":"","RepoTags":["ubuntu:12.10","ubuntu:quantal"],"RepoDigests":["ubuntu@sha256:002fba3e3255af10be97ea26e476692a7ebed0bb074a9ab960b2e7a1526b15d7","ubuntu@sha256:68ea0200f0b90df725d99d823905b04cf844f6039ef60c60bf3e019915017bd3"],"Created":1403128455,"Size":172064416,"VirtualSize":172064416,"SharedSize":0,"Labels":{},"Containers":5}]
data.frame(
  id = c(
    "sha256:e216a057b1cb1efc11f8a268f37ef62083e70b1b38323ba252e25ac88904a7e8",
    "sha256:3e314f95dcace0f5e4fd37b10862fe8398e3c60ed36600bc0ca5fda78b087175"),
  parent_id = c("", ""),
  repo_tags = I(list(c("ubuntu:12.04", "ubuntu:precise"),
                     c("ubuntu:12.10", "ubuntu:quantal"))),
  repo_digests = I(list(
    "ubuntu@sha256:992069aee4016783df6345315302fa59681aae51a8eeb2f889dea59290f21787",
    c("ubuntu@sha256:002fba3e3255af10be97ea26e476692a7ebed0bb074a9ab960b2e7a1526b15d7",
      "ubuntu@sha256:68ea0200f0b90df725d99d823905b04cf844f6039ef60c60bf3e019915017bd3"))),
  created = c(1474925151L, 1403128455L),
  size = c(103579269L, 172064416L),
  shared_size = 0L,
  virtual_size = c(103579269L, 172064416L),
  labels = I(list(character(), character())),
  containers = c(2L, 5L),
  stringsAsFactors = FALSE)
