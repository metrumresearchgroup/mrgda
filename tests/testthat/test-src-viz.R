
# This script tests all src_viz related functions

test_that("src_viz input validation", {
  # Test with non-list input
  non_list_input <- data.frame(a = 1:5, b = 6:10)
  expect_error(src_viz(non_list_input), "The input must be a list of data frames.")
})

test_that("src_viz app structure", {
  # Prepare a proper input list
  df_list <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    )
  )

  # Run the function and get the UI
  app <- src_viz(df_list)

  expect_true(inherits(app, "shiny.appobj"))
  expect_true(app$options$launch.browser)

  error_msg <- testthat::capture_error(src_viz(df_list, .subject_col = "hello"))
  expect_equal(error_msg$message, ".subject_col (hello) is not present in any dataframe")
})


test_that("check_subject_col determines the correct column", {

  # `USUBJID` is most common ID column
  df_list1 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    )
  )

  # `ID` is most common ID column
  df_list2 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      ID = 1:n()
    )
  )

  # USUBJID and ID are both present, use USUBJID
  df_list3 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    )
  )

  expect_equal(check_subject_col(df_list1), "USUBJID")
  expect_equal(check_subject_col(df_list2), "ID")
  expect_equal(check_subject_col(df_list3), "USUBJID")
})


test_that("create_global_filter creates the correct UI", {

  filter_ui <- create_global_filter(.subject_col = NULL)
  expect_equal(filter_ui, shiny::div())

  id_col <- "USUBJID"
  filter_ui <- create_global_filter(.subject_col = id_col)
  expect_true(grepl(id_col, as.character(filter_ui)))
})
