## version: 1.29
## method: delete
## path: /images/{name}
## code: 200
## response: [{"Untagged":"3e2f21a89f"},{"Deleted":"3e2f21a89f"},{"Deleted":"53b4f83ac9"}]
data.frame(
  untagged = c("3e2f21a89f", NA_character_, NA_character_),
  deleted = c(NA_character_, "3e2f21a89f", "53b4f83ac9"),
  stringsAsFactors = FALSE)
