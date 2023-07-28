test_that("v correctly modifies dataframe", {

  # Create a test dataframe - additional columns are for manually inspecting the view
  df <- data.frame(
    sex = sample(c("M", "F"), 50, replace = TRUE),
    age = sample(c(22,25,26,30,32,24), 50, replace = TRUE),
    Weight = rnorm(50, 75, 15)
  ) %>%
    dplyr::mutate(
      ID = 1:dplyr::n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ) %>%
    dplyr::relocate(ID, USUBJID)

  attr(df$USUBJID, "label") <- "Subject"

  # Test the function on the test dataframe
  result <- v(df, "USUBJID")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  expect_equal(ncol(result$x$data %>% dplyr::select(-color)), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$sex))

})

test_that("v errors for large dataset when interactive", {
  path_lg <- system.file("example-sdtm-large-lb", "lb.xpt", package = "mrgda")
  df_large <- haven::read_xpt(path_lg) %>% dplyr::slice(1:10000)

  rlang::with_interactive(value = TRUE, {
    error_msg <- testthat::capture_error(v(df_large))
  })

  expect_true(grepl(".df object size", unname(error_msg$message)))

  expect_equal(
    unname(error_msg$body),
    "Use `mrgda::src_viz(list(.df))` for large datasets, which renders the table using your R console"
  )
})

