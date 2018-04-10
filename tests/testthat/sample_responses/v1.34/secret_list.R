## version: 1.34
## method: get
## path: /secrets
## code: 200
## response: [{"ID":"blt1owaxmitz71s9v5zh81zun","Version":{"Index":85},"CreatedAt":"2017-07-20T13:55:28.678958722Z","UpdatedAt":"2017-07-20T13:55:28.678958722Z","Spec":{"Name":"mysql-passwd","Labels":{"some.label":"some.value"},"Driver":{"Name":"secret-bucket","Options":{"OptionA":"value for driver option A","OptionB":"value for driver option B"}}}},{"ID":"ktnbjxoalbkvbvedmg1urrz8h","Version":{"Index":11},"CreatedAt":"2016-11-05T01:20:17.327670065Z","UpdatedAt":"2016-11-05T01:20:17.327670065Z","Spec":{"Name":"app-dev.crt","Labels":{"foo":"bar"}}}]
data.frame(
  id = c("blt1owaxmitz71s9v5zh81zun", "ktnbjxoalbkvbvedmg1urrz8h"),
  version = I(list(list(index = 85L), list(index = 11L))),
  created_at = c("2017-07-20T13:55:28.678958722Z",
                 "2016-11-05T01:20:17.327670065Z"),
  updated_at = c("2017-07-20T13:55:28.678958722Z",
                 "2016-11-05T01:20:17.327670065Z"),
  spec = I(list(
    list(
      name = "mysql-passwd",
      labels = c(some.label = "some.value"),
      data = NA_character_,
      driver = list(
        name = "secret-bucket",
        options = c(option_a = "value for driver option A",
                    option_b = "value for driver option B"))),
    list(
      name = "app-dev.crt",
      labels = c("foo" = "bar"),
      data = NA_character_,
      driver = NULL))),
  stringsAsFactors = FALSE)
