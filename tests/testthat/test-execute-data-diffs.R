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
  execute_data_diffs(.base_df, .compare_df, temp_dir)
  diff_path <- file.path(temp_dir, "diffs.csv")
  id_diff_path <- file.path(temp_dir, "id-diffs.csv")
  on.exit(fs::file_delete(diff_path))
  on.exit(fs::file_delete(id_diff_path), add = TRUE)

  diffs <- suppressMessages(readr::read_csv(diff_path))
  expect_equal(diffs$name[1], "N Rows Diff")
  expect_equal(diffs$value[1], "1 row(s) added")
  expect_equal(diffs$name[2], "New Columns")
  expect_equal(diffs$value[2], "C")
  execute_data_diffs(.compare_df, .base_df, temp_dir)
  diffs <- suppressMessages(readr::read_csv(diff_path))
  expect_equal(diffs$value[4], "1 ID(s) removed")
})

# 2. Test: The function handles non-existent directories correctly.
test_that("The function throws error for non-existent directories [NMV-EDD-002]", {
  expect_error(execute_data_diffs(.base_df, .compare_df, "non_existent_directory"))
})

# 3. Test: The function correctly writes the output files.
test_that("The function writes the output files [NMV-EDD-003]", {
  execute_data_diffs(.base_df, .compare_df, temp_dir)
  diff_path <- file.path(temp_dir, "diffs.csv")
  id_diff_path <- file.path(temp_dir, "id-diffs.csv")
  on.exit(fs::file_delete(diff_path))
  on.exit(fs::file_delete(id_diff_path), add = TRUE)
  expect_true(fs::file_exists(diff_path))
  expect_true(fs::file_exists(id_diff_path))
})

# 4. Test: The function properly identifies and outputs differences based on IDs.
test_that("The function identifies and outputs differences based on IDs [NMV-EDD-004]", {
  execute_data_diffs(.base_df, .compare_df, temp_dir)
  id_diffs <- suppressMessages(readr::read_csv(file.path(temp_dir, "id-diffs.csv")))
  expect_equal(nrow(id_diffs), 1)
  expect_equal(id_diffs$ID[1], 3)
  expect_equal(id_diffs$BASE[1], 2.5)
  expect_equal(id_diffs$COMPARE[1], 3)
})

# 4. Test: The function properly identifies and outputs differences based on IDs.
test_that("The function returns nothing if there are no diffs detected", {
  df <- execute_data_diffs(.base_df, .base_df, temp_dir)
  expect_true(is.null(df))
  expect_message(execute_data_diffs(.base_df, .base_df, temp_dir), "No diffs since last version found")
})

test_that("The function uses .id_col for grouping correctly", {
  diff_path <- file.path(temp_dir, "diffs.csv")
  id_diff_path <- file.path(temp_dir, "id-diffs.csv")
  on.exit(fs::file_delete(diff_path))
  on.exit(fs::file_delete(id_diff_path), add = TRUE)

  # With diffs
  execute_data_diffs(.base_df2, .compare_df2, temp_dir, .id_col = "USUBJID")
  df <- suppressMessages(readr::read_csv(file.path(temp_dir, "id-diffs.csv")))
  expect_true("USUBJID" %in% names(df))

  # With no diffs (creates/saves out csv differently)
  execute_data_diffs(.base_df2, .base_df2, temp_dir, .id_col = "USUBJID")
  df <- suppressMessages(readr::read_csv(file.path(temp_dir, "id-diffs.csv")))
  expect_true("USUBJID" %in% names(df))
})


test_that("The function doesn't save csvs if .output_dir is NULL", {
  lst <- execute_data_diffs(.base_df2, .compare_df2, .output_dir = NULL, .id_col = "USUBJID")
  diff_path <- file.path(temp_dir, "diffs.csv")
  id_diff_path <- file.path(temp_dir, "id-diffs.csv")
  expect_true(!fs::file_exists(diff_path))
  expect_true(!fs::file_exists(id_diff_path))

  # Also test that objects are invisibly returned
  expect_false(rlang::is_empty(lst$summary_diffs))
  expect_false(rlang::is_empty(lst$variable_diffs))
})

test_that("The function works if .id_col is NULL or not present", {
  # .id_col is NULL
  lst <- execute_data_diffs(.base_df2, .compare_df2, .output_dir = NULL, .id_col = NULL)
  expect_false(rlang::is_empty(lst$summary_diffs))
  expect_false(rlang::is_empty(lst$variable_diffs))
  expect_false("USUBJID" %in% names(lst$variable_diffs))

  # .id_col is a string not present in the datasets
  expect_warning(
    lst <- execute_data_diffs(.base_df2, .compare_df2, .output_dir = NULL, .id_col = "ID"),
    "The specified `.id_col`"
  )
  expect_false(rlang::is_empty(lst$summary_diffs))
  expect_false(rlang::is_empty(lst$variable_diffs))
  expect_false("USUBJID" %in% names(lst$variable_diffs))
})
