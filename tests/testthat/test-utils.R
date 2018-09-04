context("test-utils.R")

test_that("Functions in utils work", {
  expect_silent(create_this_week(path = tempdir()))
})
