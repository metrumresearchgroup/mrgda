# Create temp directory to put history in
history_dir <- tempdir()

history_prev <- dplyr::tibble(
    Author = "John",
    Comment = "Initial commit",
    `Previous Revision` = "",
    Datetime = "2022-01-01")

# Save history to temp directory
readr::write_csv(history_prev, file = file.path(history_dir, "history.csv"))

test_that("gather_data_history adds a new row to history", {

  cur_history <-
    gather_data_history(
      history_prev,
      .comment = "Updated data",
      .meta_data_folder = history_dir,
      .prev_rev = "54"
    )

  # Check size of history didn't add columns and only added 1 row
  expect_equal(nrow(history_prev)+1, nrow(cur_history))
  expect_true(ncol(cur_history)==4)

  # Check author captured correctly
  expect_true(cur_history$Author[1] == Sys.info()[["user"]])

  # Check comments passed through correctly
  expect_true(cur_history$Comment[1] == "Updated data")
  expect_true(cur_history$Comment[2] == "Initial commit")

  # Check previous revision
  expect_true(cur_history$`Previous Revision`[1] == "54")
  expect_true(cur_history$`Previous Revision`[2] == "")

})
