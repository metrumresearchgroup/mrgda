# Create temporary directory for testing
temp_dir <- tempdir()

# Create test data frames
.base_df <- data.frame(
  ID = c(1, 2, 3),
  A = c("a", "b", "c"),
  B = c(1, 2, 2.5)
)

.compare_df <- data.frame(
  ID = c(1, 2, 3, 4),
  A = c("a", "b", "c", "d"),
  B = c(1, 2, 3, 4),
  C = c(4, 5, 6, 7)
)

.base_df2 <- .base_df %>% dplyr::rename(USUBJID=ID)
.compare_df2 <- .compare_df %>% dplyr::rename(USUBJID=ID)

# 1. Test: The function properly identifies and outputs differences in the data frames.
test_that("The function identifies and outputs differences", {
  diffs_list <- execute_data_diffs(.base_df, .compare_df, "ID")
  diffs <- diffs_list$diffs
  expect_equal(diffs$name[1], "N Rows Diff")
  expect_equal(diffs$value[1], "1 row(s) added")
  expect_equal(diffs$name[2], "New Columns")
  expect_equal(diffs$value[2], "C")
  expect_equal(diffs$value[5], "1 ID(s) added")
})


test_that("The function returns nothing if there are no diffs detected", {
  df <- execute_data_diffs(.base_df, .base_df, "ID")
  expect_true(nrow(df$diffs) == 0)
  expect_message(execute_data_diffs(.base_df, .base_df, "ID"), "No diffs since last version found")
})



test_that("The function works if .suject_col is NULL", {
  lst <- execute_data_diffs(.base_df2, .compare_df2, .subject_col = NULL)
  expect_false(rlang::is_empty(lst$diffs))
  expect_equal(lst$value_diffs$BASE[1], "2.5")

  x <-  try(execute_data_diffs(.base_df2, .compare_df2, .subject_col = "ID"), silent = TRUE)

  expect_equal(
    as.character(x),
    "Error in execute_data_diffs(.base_df2, .compare_df2, .subject_col = \"ID\") : \n  The specified `.subject_col` (ID) is not present in one or both of the data frames\n"
  )

})

test_that("Test if data.frames don't have shared names", {
  base_df <- data.frame(A = c(1, 2), B = c(4, 7), C = c(5, 9))
  compare_df <- data.frame(D = c(1, 2), E = c(4, 7), F = c(5, 9))

  expect_error(
    execute_data_diffs(base_df, compare_df,
                       "The base and compare data frames do not have any columns to compare"))
})

# Create test data frames
.compare_df3 <- data.frame(
  ID = c(1, 2, 3),
  A = c("a", "b", "c"),
  B = c(1, 2, 2.5)
)

.base_df3 <- data.frame(
  ID = c(1, 2, 3, 4),
  A = c("a", "b", "c", "d"),
  B = c(1, 2, 3, 4),
  C = c(4, 5, 6, 7)
)

test_that("execute_data_diffs works with removing columns", {
  diffs_list3 <- execute_data_diffs(.base_df3, .compare_df3, "ID")

  diffs <- diffs_list3$diffs
  expect_equal(diffs$value[1], "1 row(s) removed")
  expect_equal(diffs$name[2], "Removed Columns")
  expect_equal(diffs$value[2], "C")
})

