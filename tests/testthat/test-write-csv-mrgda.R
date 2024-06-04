test_that("write_csv_dots works with typical case", {
  tmp_file <- tempfile(fileext = ".csv")

  write_csv_dots(x = mtcars, file = tmp_file)

  expect_true(file.exists(tmp_file))

  mtcars2 <- mtcars

  mtcars2$cyl[1] <- NA_real_

  write_csv_dots(x = mtcars2, file = tmp_file, na = "Missing")

  mtcars2_read <- read_csv_dots(tmp_file)

  expect_true(mtcars2_read$cyl[1] == "Missing")
})
