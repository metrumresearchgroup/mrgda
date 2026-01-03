path <- system.file("example-sdtm", package = "mrgda")


test_that("list_files_of_type: check xpt works in test dir", {
  expect_equal(basename(list_files_of_type(path, "xpt")$files_of_type[1]), "ae.xpt")
})

test_that("list_files_of_type: check sas7bdat does not work in test dir", {
  expect_equal(length(list_files_of_type(path, "sas7bdat")$files_of_type), 0)
})

test_that("list_files_of_type errors if file type not csv, xpt or sas7bdat", {
  expect_error(
    list_files_of_type(path, "detect"),
    "'.file_types' must be 'csv', 'sas7bdat', or 'xpt'"
  )
})

test_that("list_files_of_type respects user-specified file type", {

  if (!dir.exists(file.path(tempdir(), "listfiletype"))) {
    dir.create(file.path(tempdir(), "listfiletype"))
  }

  write_csv_dots(data.frame(A = c(1,2), B = c(3,4)), file = file.path(tempdir(), "listfiletype/example.csv"))
  write_csv_dots(data.frame(A = c(1,2), B = c(3,4)), file = file.path(tempdir(), "listfiletype/example2.csv"))
  haven::write_xpt(data.frame(A = c(1,2), B = c(3,4)), file.path(tempdir(), "listfiletype/example.xpt"))

  file_type_df <- list_files_of_type(file.path(tempdir(), "listfiletype"), .file_types = "csv") %>% suppressMessages()
  expect_true(file_type_df$type == "csv")
  expect_true(length(file_type_df$files_of_type) == 2)

})
