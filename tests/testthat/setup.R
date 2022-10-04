library(dplyr)
library(testthat)
library(mrgda)

nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
nm_errors <- readr::read_csv(system.file("derived", "pk-errors.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
n_tests <- length(nm_validate(nm, nm_spec))

nm_spec_noflags <- yspec::ys_load(system.file("derived", "pk-noflags.yml", package = "mrgda"))
nm_spec_parflags <- yspec::ys_load(system.file("derived", "pk-partial_flags.yml", package = "mrgda"))

# Check each err message where:
# .res=df with err, .i= failure num, .err_row = num rows in err output df, .desc = failure description
check_single_error <- function(.res, .i, .err_row, .desc, .err_type = "FAILURE"){
  expect_equal(
    sum(purrr::map_lgl(.res, ~ .x$success)),
    length(.res) - 1
  )
  if(.err_type == "FAILURE"){
    expect_false(.res[[.i]]$success)
  }
  expect_true(nrow(.res[[.i]]$error_content) == .err_row)
  expect_true(any(grepl(
    .desc,
    .res[[.i]]$description
  )))
}

grepl_fixed <- function(pattern, x){
  grepl(pattern, x, fixed = TRUE)
}
