context("test-help.R")

my_spec <- system.file("specs/thisweek.yml", package = "datalang")
my_spec_path <- system.file("specs", package = "datalang")
my_translation <- system.file("translations", package = "datalang")

thisweek2 <- thisweek
thisweek2$day <- as.character(thisweek2$day)

test_that("help stuff works",{
  expect_silent(create_html_help(my_spec))
  expect_silent(datalang_help_add("thisweek", my_spec))
  expect_silent(datalang_help("thisweek"))
})
