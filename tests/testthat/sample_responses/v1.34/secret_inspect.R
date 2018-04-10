## version: 1.34
## method: get
## path: /secrets/{id}
## code: 200
## response: {"ID":"ktnbjxoalbkvbvedmg1urrz8h","Version":{"Index":11},"CreatedAt":"2016-11-05T01:20:17.327670065Z","UpdatedAt":"2016-11-05T01:20:17.327670065Z","Spec":{"Name":"app-dev.crt","Labels":{"foo":"bar"},"Driver":{"Name":"secret-bucket","Options":{"OptionA":"value for driver option A","OptionB":"value for driver option B"}}}}
list(
  id = "ktnbjxoalbkvbvedmg1urrz8h",
  version = list(index = 11L),
  created_at = "2016-11-05T01:20:17.327670065Z",
  updated_at = "2016-11-05T01:20:17.327670065Z",
  spec = list(
    name = "app-dev.crt",
    labels = c("foo" = "bar"),
    data = NA_character_,
    driver = list(
      name = "secret-bucket",
      options = c(option_a = "value for driver option A",
                  option_b = "value for driver option B"))))
