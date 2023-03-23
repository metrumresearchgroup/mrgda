path <- system.file("example-sdtm", package = "mrgda")


test_that("list_files_of_type: check detect works", {
  expect_equal(list_files_of_type(path, "detect")$type, "xpt")
})

test_that("list_files_of_type: check xpt works", {
  expect_equal(basename(list_files_of_type(path, "xpt")$files_of_type[1]), "ae.xpt")
})

test_that("list_files_of_type: check sas7bdat does not work", {
  expect_equal(length(list_files_of_type(path, "sas7bdat")$files_of_type), 0)
})
