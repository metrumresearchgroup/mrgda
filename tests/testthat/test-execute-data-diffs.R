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
test_that("The function identifies and outputs differences [NMV-EDD-001]", {
  diffs_list <- execute_data_diffs(.base_df, .compare_df, "ID")
  diffs <- diffs_list$diffs
  expect_equal(diffs$name[1], "N Rows Diff")
  expect_equal(diffs$value[1], "1 row(s) added")
  expect_equal(diffs$name[2], "New Columns")
  expect_equal(diffs$value[2], "C")
  expect_equal(diffs$value[5], "1 ID(s) added")
})


# 4. Test: The function properly identifies and outputs differences based on IDs.
test_that("The function identifies and outputs differences based on IDs [NMV-EDD-004]", {
  diffs <- execute_data_diffs(.base_df, .compare_df, "ID")
  id_diffs <- diffs$subject_diffs
  expect_equal(nrow(id_diffs), 1)
  expect_equal(id_diffs$ID[1], "3")
  expect_equal(id_diffs$BASE[1], "2.5")
  expect_equal(id_diffs$COMPARE[1], "3")
})

# 4. Test: The function properly identifies and outputs differences based on IDs.
test_that("The function returns nothing if there are no diffs detected", {
  df <- execute_data_diffs(.base_df, .base_df, "ID")
  expect_true(nrow(df$diffs) == 0)
  expect_true(nrow(df$subject_diffs) == 0)
  expect_message(execute_data_diffs(.base_df, .base_df, "ID"), "No diffs since last version found")
})



test_that("The function works if .suject_col is NULL", {
  lst <- execute_data_diffs(.base_df2, .compare_df2, .subject_col = NULL)
  expect_false(rlang::is_empty(lst$diffs))
  expect_equal(lst$value_diffs$BASE[1], "2.5")
  expect_true(is.null(lst$subject_diffs))

  x <-  try(execute_data_diffs(.base_df2, .compare_df2, .subject_col = "ID"), silent = TRUE)

  expect_equal(
    as.character(x),
    "Error in execute_data_diffs(.base_df2, .compare_df2, .subject_col = \"ID\") : \n  The specified `.subject_col` (ID) is not present in one or both of the data frames\n"
  )

})
