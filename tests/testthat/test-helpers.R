context("helpers")


test_that("user", {
  skip_on_windows()
  id <- user()
  expect_is(id, "character")
  expect_match(id, "^[0-9]+$")
  expect_equal(id, system3("id", "-u")$output)
})


test_that("user, width group", {
  skip_on_windows()
  id <- user(group = TRUE)
  expect_is(id, "character")
  expect_match(id, "^[0-9]+:[0-9]+$")
  expect_equal(id, sprintf("%s:%s",
                           system3("id", "-u")$output,
                           system3("id", "-g")$output))
})


test_that("pick user", {
  skip_on_windows()
  expect_equal(user("root"), "0")
  expect_equal(user("root", TRUE), "0:0")
})


test_that("missing user", {
  skip_on_windows()
  nm <- rand_str()
  expect_error(user(nm), "'id' failed with message:")
})


test_that("no numeric_id on windows", {
  expect_error(numeric_id(NULL, FALSE, TRUE),
               "Cannot return numeric id on windows")
  expect_error(numeric_id(NULL, TRUE, TRUE),
               "Cannot return numeric id on windows")
})


test_that("user_name", {
  expect_identical(user_name("rich"), "rich")

  withr::with_envvar(
    c(LOGNAME = NA, USER = NA, LNAME = NA, USERNAME = NA),
    expect_error(user_name(NULL), "could not determine username"))
  withr::with_envvar(
    c(LOGNAME = NA, USER = NA, LNAME = NA, USERNAME = "a"),
    expect_identical(user_name(NULL), "a"))
  withr::with_envvar(
    c(LOGNAME = NA, USER = NA, LNAME = "b", USERNAME = "a"),
    expect_identical(user_name(NULL), "b"))
  withr::with_envvar(
    c(LOGNAME = NA, USER = "c", LNAME = "b", USERNAME = "a"),
    expect_identical(user_name(NULL), "c"))
  withr::with_envvar(
    c(LOGNAME = "d", USER = "c", LNAME = "b", USERNAME = "a"),
    expect_identical(user_name(NULL), "d"))

  withr::with_envvar(
    c(LOGNAME = "d", USER = "c", LNAME = "b", USERNAME = "a"),
    expect_identical(user(NULL, FALSE, FALSE), "d"))
})
