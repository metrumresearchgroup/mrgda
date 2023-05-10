# Helper function for generating dummy data
generate_dummy_data <- function(n = 5, same_id = FALSE) {
  data.frame(
    ID = if (same_id)
      rep(1:n / 2, each = 2)
    else
      1:n,
    NUM = rnorm(n),
    VAR = rnorm(n)
  )
}

read_file <- function(file) {
  readLines(file, warn = FALSE)
}


test_that("Error when directory does not exist", {
  dir <- "non_existing_directory"
  expect_error(
    execute_data_diffs(generate_dummy_data(), generate_dummy_data(), dir),
    paste(dir, "does not exist")
  )
})



test_that("No differences with identical data frames", {
  dir <- tempdir()
  base_df <- generate_dummy_data()
  compare_df <- base_df

  execute_data_diffs(base_df, compare_df, dir)

  # Check output files
  diff_file <- read_file(file.path(dir, "data-diff.txt"))
  expect_equal("No issues were found!", diff_file)
})


test_that("Differences detected with different data frames", {
  dir <- tempdir()
  base_df <- generate_dummy_data()
  compare_df <- generate_dummy_data()  # Different data

  execute_data_diffs(base_df, compare_df, dir)

  # Check output files
  diff_file <- read_file(file.path(dir, "data-diff.txt"))
  expect_true(length(diff_file) > 0) # File should not be empty
  expect_true(any(grepl("Differences", diff_file))) # There should be lines indicating differences
})


test_that("Differences detected for the same 'ID'", {
  dir <- tempdir()
  base_df <- generate_dummy_data(same_id = TRUE)
  compare_df <-
    generate_dummy_data(same_id = TRUE)  # Different data, same 'ID'

  execute_data_diffs(base_df, compare_df, dir)

  # Check output files
  diff_file <- read_file(file.path(dir, "data-diff.txt"))
  expect_true(length(diff_file) > 0) # File should not be empty
  expect_true(any(grepl("Differences", diff_file))) # There should be lines indicating differences

  id_diff_df <- read.csv(file.path(dir, "id-diffs.csv"))
  expect_true(nrow(id_diff_df) > 0)
  expect_true(inherits(id_diff_df, "data.frame")) # There should be lines indicating differences
})
