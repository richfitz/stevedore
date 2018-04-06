## version: 1.26
## method: get
## path: /secrets/{id}
## code: 200
## response: {"ID":"ktnbjxoalbkvbvedmg1urrz8h","Version":{"Index":11},"CreatedAt":"2016-11-05T01:20:17.327670065Z","UpdatedAt":"2016-11-05T01:20:17.327670065Z","Spec":{"Name":"app-dev.crt"}}
list(
  id = "ktnbjxoalbkvbvedmg1urrz8h",
  version = list(index = 11L),
  created_at = "2016-11-05T01:20:17.327670065Z",
  updated_at = "2016-11-05T01:20:17.327670065Z",
  spec = list(
    name = "app-dev.crt",
    labels = NULL,
    task_template = NULL,
    mode = NULL,
    update_config = NULL,
    networks = data.frame(target = character(0),
                          aliases = I(list()),
                          stringsAsFactors = FALSE),
    endpoint_spec = NULL))
