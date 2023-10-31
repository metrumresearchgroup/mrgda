history_prev <- dplyr::tibble(
    Author = "John Smith",
    `N Row` = "340",
    `N Col` = "16",
    `N ID` = "50",
    `Previous Revision` = "",
    Comment = "Initial commit",
    `Datetime (EDT)` = "2023-10-06 09:58:58"
    )

cur_history <-
  gather_data_history(
    .cur_data = Theoph %>% dplyr::rename(ID = Subject),
    .cur_history = history_prev,
    .comment = "Update to Theoph",
    .prev_rev = "54")

# Save history to temp directory
#readr::write_csv(history_prev, file = file.path(history_dir, "history.csv"))

test_that("gather_data_history adds a new row to history", {

  # Check size of history didn't add columns and only added 1 row
  expect_equal(nrow(history_prev)+1, nrow(cur_history))
  expect_true(ncol(cur_history)==7)

  # Check author captured correctly
  expect_true(cur_history$Author[1] == Sys.info()[["user"]])

  # Check comments passed through correctly
  expect_true(cur_history$Comment[1] == "Update to Theoph")
  expect_true(cur_history$Comment[2] == "Initial commit")

  # Check previous revision
  expect_true(cur_history$`Previous Revision`[1] == "54")
  expect_true(cur_history$`Previous Revision`[2] == "")

  # Check ID counts
  expect_true(cur_history$`N ID`[1] == "12")
  expect_true(cur_history$`N Row`[1] == "132")
  expect_true(cur_history$`N Col`[1] == "5")

})

mtcars_history <-
  gather_data_history(
    .cur_data = mtcars,
    .cur_history = cur_history,
    .comment = "Update to mtcars",
    .prev_rev = "102")

test_that("gather_data_history works for third update", {

  # Check size of history didn't add columns and only added 1 row
  expect_true(ncol(mtcars_history)==7)
  expect_true(nrow(mtcars_history)==3)

  # Check author captured correctly
  expect_true(mtcars_history$Author[1] == Sys.info()[["user"]])

  # Check comments passed through correctly
  expect_true(mtcars_history$Comment[1] == "Update to mtcars")

  # Check previous revision
  expect_true(mtcars_history$`Previous Revision`[1] == "102")

  # Check data stats
  expect_true(mtcars_history$`N Row`[1] == "32")
  expect_true(mtcars_history$`N Col`[1] == "11")
  expect_true(mtcars_history$`N ID`[1] == "0")
})
