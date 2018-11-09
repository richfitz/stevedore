## version: 1.38
## method: get
## path: /configs
## code: 200
## response: [{"ID":"ktnbjxoalbkvbvedmg1urrz8h","Version":{"Index":11},"CreatedAt":"2016-11-05T01:20:17.327670065Z","UpdatedAt":"2016-11-05T01:20:17.327670065Z","Spec":{"Name":"server.conf","Data":"VEhJUyBJUyBOT1QgQSBSRUFMIENFUlRJRklDQVRFCg=="}}]
data.frame(
  id = "ktnbjxoalbkvbvedmg1urrz8h",
  version = I(list(list(index = 11L))),
  created_at = "2016-11-05T01:20:17.327670065Z",
  updated_at = "2016-11-05T01:20:17.327670065Z",
  spec = I(list(list(
    name = "server.conf",
    labels = NULL,
    ## NOTE: in the online api docs:
    ## https://docs.docker.com/engine/api/v1.30/#operation/ConfigInspect
    ## the 'data' element is not returned, which is not consistent
    ## with the spec.
    data = "VEhJUyBJUyBOT1QgQSBSRUFMIENFUlRJRklDQVRFCg==",
    templating = NULL))),
  stringsAsFactors = FALSE)
