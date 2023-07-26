test_that("view_src correctly modifies dataframe", {

  # Create a test dataframe - additional columns are for manually inspecting the view
  df <- data.frame(
    sex = sample(c("M", "F"), 50, replace = TRUE),
    age = sample(c(22,25,26,30,32,24), 50, replace = TRUE),
    Weight = round(rnorm(50, 75, 15), 1)
  ) %>%
    dplyr::mutate(
      ID = 1:dplyr::n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n()),
      notes = rep("this is a very long line that should probably be wrapped", dplyr::n())
      ) %>%
    dplyr::relocate(ID, USUBJID)

  attr(df$USUBJID, "label") <- "Subject"

  # Test the function on the test dataframe
  result <- view_src(df, "USUBJID", .view = "viewer")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  expect_equal(ncol(result$x$data %>% dplyr::select(-color)), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$sex))

})

