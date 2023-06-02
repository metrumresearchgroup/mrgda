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
  B = c(1, 2, 3, 4)
)

# 1. Test: The function properly identifies and outputs differences in the data frames.
test_that("The function identifies and outputs differences [NMV-EDD-001]", {
  execute_data_diffs(.base_df, .compare_df, temp_dir)
  diffs <- suppressMessages(readr::read_csv(file.path(temp_dir, "diffs.csv")))
  expect_equal(diffs$name[1], "N Rows Diff")
  expect_equal(diffs$value[1], "1 row(s) added")
  execute_data_diffs(.compare_df, .base_df, temp_dir)
  diffs <- suppressMessages(readr::read_csv(file.path(temp_dir, "diffs.csv")))
  expect_equal(diffs$value[3], "1 ID(s) removed")
})

# 2. Test: The function handles non-existent directories correctly.
test_that("The function throws error for non-existent directories [NMV-EDD-002]", {
  expect_error(execute_data_diffs(.base_df, .compare_df, "non_existent_directory"))
})

# 3. Test: The function correctly writes the output files.
test_that("The function writes the output files [NMV-EDD-003]", {
  execute_data_diffs(.base_df, .compare_df, temp_dir)
  expect_true(file.exists(file.path(temp_dir, "diffs.csv")))
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
