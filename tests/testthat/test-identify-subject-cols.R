test_that("Identifies columns constant within subjects", {
  df <- data.frame(
    subject = c(1, 1, 2, 2),
    z = c("a", "a", "a", "a"),
    x = c(5, 5, 5, 5),
    y = c(1, 2, 1, 2)
  )
  result <- identify_subject_cols(df, "subject")
  expect_equal(sort(result), c("x", "z"))
})

test_that("Returns none when no columns are constant", {
  df <- data.frame(
    subject = c(1, 1, 2, 2),
    x = c(1, 2, 3, 4),
    y = c(5, 6, 7, 8)
  )
  result <- identify_subject_cols(df, "subject")
  expect_equal(result, "none")
})

test_that("Handles missing values appropriately", {
  df <- data.frame(
    subject = c(1, 1, 2, 2),
    x = c(NA, NA, NA, NA),
    y = c(1, 1, NA, NA),
    z = c(1, 1, 1, 1),
    a = c(1, NA, 1, 1)
  )
  result <- identify_subject_cols(df, "subject")
  expect_equal(sort(result), c("x", "y", "z"))
})

test_that("Handles various data types", {
  df <- data.frame(
    subject = c(1, 1, 2, 2),
    num = c(5, 5, 5, 5),
    char = c("a", "a", "b", "b"),
    factor = factor(c("d", "d", "e", "e")),
    logical = c(TRUE, TRUE, FALSE, FALSE)
  )
  result <- identify_subject_cols(df, "subject")
  expect_equal(sort(result), c("char", "factor", "logical", "num"))
})

test_that("Errors on nonexistent subject column", {
  df <- data.frame(
    subject = c(1, 1, 2, 2),
    x = c(1, 2, 3, 4)
  )
  expect_error(
    identify_subject_cols(df, "nonexistent_col"),
    "Subject column 'nonexistent_col' does not exist in the data frame."
  )
})
