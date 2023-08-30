

# Test if function throws an error when the input is not a diffdf object
test_that("Function correctly throws an error for non-diffdf input", {
  expect_error(diffdf_value_changes_to_df(data.frame(a = 1:5, b = 6:10)),
               ".diffdf_obj must be a diffdf object")
})

# Test if function correctly returns NULL for diffdf object without any VarDiff_ fields
test_that("Function correctly returns NULL for diffdf object without VarDiff_ fields",
          {
            df1 <- data.frame(a = 1:5, b = 6:10)
            df2 <- data.frame(a = 1:5, b = 6:10)
            diff_df <- diffdf::diffdf(df1, df2)

            expect_null(diffdf_value_changes_to_df(diff_df))
          })

# Test if function correctly returns a dataframe for diffdf object with VarDiff_ fields
test_that("Function correctly returns a dataframe for diffdf object with VarDiff_ fields",
          {
            df1 <- data.frame(a = 1:2, b = 6:7)
            df2 <- data.frame(a = 1:2, b = 7:8)
            diff_df <- diffdf::diffdf(df1, df2, suppress_warnings = TRUE)

            expect_true(
              all(
                diffdf_value_changes_to_df(diff_df) ==
                dplyr::tribble(
                  ~VARIABLE, ~ROWNUMBER, ~BASE, ~COMPARE,
                  "b", "1", "6", "7",
                  "b", "2", "7", "8",
                )
              )
            )

          })
