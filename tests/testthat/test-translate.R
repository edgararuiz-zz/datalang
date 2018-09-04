context("test-translate.R")

my_spec <- system.file("specs/thisweek.yml", package = "datalang")
my_spec_path <- system.file("specs", package = "datalang")
my_translation <- system.file("translations", package = "datalang")

thisweek2 <- thisweek
thisweek2$day <- as.character(thisweek2$day)

test_that("Translated data frame matches expectations", {
  expect_equal(
    colnames(translate_data(my_spec)),
    c("dia","manana","tarde")
  )
  expect_equal(
    colnames(translate_data(my_spec, .data = thisweek)),
    c("dia","manana","tarde")
  )
  expect_equal(
    colnames(translate_data(my_spec, .data = thisweek)),
    c("dia","manana","tarde")
  )
  expect_equal(
    colnames(translate_data(my_spec, .data = thisweek2)),
    c("dia","manana","tarde")
  )
  expect_equal(
    colnames(translate_data(my_spec, .data = tibble::as_tibble(thisweek))),
    c("dia","manana","tarde")
  )
  expect_equal(
    colnames(translate_data(my_spec, .data = thisweek)),
    c("dia","manana","tarde")
  )
  expect_equal(
    as.character(thisweek[[1]]),
    c("friday", "saturday", "sunday" )
  )
})


test_that("Save works", {
  expect_silent(save_translation(my_spec, data_folder = tempdir()))
  expect_silent(load_translation(my_spec))
  expect_silent(load_folder_data(my_spec_path))
  expect_silent(folder_data(my_spec_path, data_folder = tempdir()))
  expect_output(
    load_package_translations(my_translation, language = "es"),
    "El lenguaje asignado"
    )
  expect_silent(translate_folder(
    my_spec_path,
    data_folder = tempdir(),
    rd_folder = tempdir()
    ))
})
