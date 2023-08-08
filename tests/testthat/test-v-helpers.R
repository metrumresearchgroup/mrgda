

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

