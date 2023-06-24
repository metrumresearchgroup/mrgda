test_that("view_src correctly modifies dataframe", {

  # Create a test dataframe
  df <- data.frame(
    USUBJID1 = c(1,2,3,4,5,6),
    sex = c("M", "F", "F", "M", "M", "F"),
    age = c(22,25,26,30,32,24)
  )

  attr(df$USUBJID1, "label") <- "Subject"

  # Test the function on the test dataframe
  result <- view_src(df, "USUBJID1")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  expect_equal(ncol(result$x$data), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$`<b>USUBJID1</b><br><i>Subject</i>`))

})

