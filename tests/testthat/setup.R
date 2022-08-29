library(dplyr)

nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "nmvalidate"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "nmvalidate"), na = ".", show_col_types = FALSE)
nm_errors <- readr::read_csv(system.file("derived", "pk-errors.csv", package = "nmvalidate"), na = ".", show_col_types = FALSE)

# Check each err message where:
# .res=df with err, .i= failure num, .err_row = num rows in err output df, .desc = failure description
check_single_error <- function(.res, .i, .err_row, .desc, .err_type = "FAILURE"){
  expect_equal(
    sum(purrr::map_lgl(.res, ~ .x$success)),
    length(.res) - 1
  )
  if(.err_type == "FAILURE"){
    expect_false(.res[[.i]]$success)
    expect_true(.res[[.i]]$critical)
  }

  if(.err_type == "WARNING"){
    expect_false(.res[[.i]]$success)
    expect_false(.res[[.i]]$critical)
  }

  expect_true(nrow(.res[[.i]]$error_content) == .err_row)
  expect_true(any(grepl(
    .desc,
    .res[[.i]]$description
  )))
}
