

test_that("gather_v_cols determines the correct subject and frozen columns", {


  get_freeze_cols <- function(df_list){
    unique_cols <- names(table(unlist(purrr::map(df_list, ~ names(.x)))))
    unique_cols[!(unique_cols %in% c("USUBJID", "ID"))]
  }

  # `USUBJID` is most common ID column
  df_list1 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
  )

  # `ID` is most common ID column
  df_list2 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      ID = 1:n()
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
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
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
  )

  v_cols <- gather_v_cols(df_list1)
  expect_equal(v_cols$.subject_col, "USUBJID")

  v_cols <- gather_v_cols(df_list2)
  expect_equal(v_cols$.subject_col, "ID")

  v_cols <- gather_v_cols(df_list3)
  expect_equal(v_cols$.subject_col, "USUBJID")


  # Subtitle and freeze calls are the same for all example lists - test once
  expect_equal(v_cols$.freeze_cols$subtitle, paste0("(", c(rep("c", 8), "a", "b"), ")"))
  expect_equal(
    v_cols$.freeze_cols$col_name,
    get_freeze_cols(df_list3)
  )

  # Subject column not found
  error_msg <- testthat::capture_error(gather_v_cols(df_list1, .subject_col = "hello"))
  expect_equal(error_msg$message, ".subject_col (hello) is not present in any dataframe")
})


test_that("create_global_filter creates the correct UI", {

  filter_ui <- create_global_filter(.subject_col = NULL)
  expect_equal(filter_ui, shiny::div())

  id_col <- "USUBJID"
  filter_ui <- create_global_filter(.subject_col = id_col)
  expect_true(grepl(id_col, as.character(filter_ui)))
})


test_that("make_v_caption works correctly for various .subject_col specifications", {
  df <- create_test_v_df(rows_per_id = 3, num_ids = 21)
  cap <- make_v_caption("test", df)
  expect_equal(cap$name, "test")
  expect_equal(cap$n_subs, 0)
  expect_equal(cap$label, "test No subjects detected")

  cap <- make_v_caption("test", df, .subject_col = "USUBJID")
  expect_equal(cap$n_subs, 21)
  expect_equal(cap$label, "test (N USUBJID: 21)")

  cap <- make_v_caption("test", df, .subject_col = "ID")
  expect_equal(cap$n_subs, 21)
  expect_equal(cap$label, "test (N ID: 21)")
})



test_that("filter_v_subject filters using global subject filter", {
  df <- create_test_v_df(rows_per_id = 3, num_ids = 21)

  # Test filtering
  filter_df <- filter_v_subject(df, "USUBJID", "3053-4")
  expect_equal(nrow(filter_df), 3)
  expect_true(all(grepl("3053-4", filter_df$USUBJID)))

  # Test if none found
  filter_df <- filter_v_subject(df, "USUBJID", "3053-100")
  expect_equal(nrow(filter_df), 1)
  expect_true(grepl("No subjects found", filter_df$USUBJID))
  expect_true(
    filter_df %>% dplyr::select(-"USUBJID") %>% t() %>%
      data.frame() %>% unlist() %>% unique() %>% is.na()
  )
})
