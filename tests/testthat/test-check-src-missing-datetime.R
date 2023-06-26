src_list <- mrgda::read_src_dir(system.file("example-sdtm", package = "mrgda")) %>% suppressMessages()

test_that("check_src_missing_datetime works with multiple source domains", {

  ae_res <- check_src_missing_datetime(.domain_df = src_list$ae, .domain_name = "ae")
  expect_true(ae_res$ColsCheck == "AESTDTC")
  expect_true(ae_res$Result == "Fail")
  expect_equal(ae_res$`N fail (%)`, "16 (100)")

  lb_res <- check_src_missing_datetime(.domain_df = src_list$lb, .domain_name = "lb")
  expect_true(lb_res$ColsCheck == "LBDTC")
  expect_true(lb_res$Result == "Pass")

  vs_res <- check_src_missing_datetime(.domain_df = src_list$vs, .domain_name = "vs")
  expect_true(vs_res$ColsCheck == "VSDTC")
  expect_true(vs_res$Result == "Fail")
  expect_equal(vs_res$`N fail (%)`, "72 (100)")
})

df_test <-
  dplyr::tibble(
    USUBJID = c("a", "a", "b", "c", "d", "d"),
    TIME = c(NA_character_, "2022-02-01T05:09", "2022-02-01T05:09:16", "2022-02-01T05", "2022-02-01T05:09:30", ""),
    DVID = 1
  )

test_that("check_src_duplicates returns expected output", {
  test_res <- check_src_missing_datetime(.domain_df = df_test, .domain_name = "test_df", .time_col = "TIME", .subject_col = "USUBJID")
  expect_true(test_res$Result == "Fail")
  expect_true(test_res$ColsCheck == "TIME")
  expect_true(test_res[["N fail (%)"]] == "3 (50)")

  expect_true(nrow(test_res$IssueRecords) == 3)
  expect_true(all(unique(test_res$IssueRecords$MRGDA_ISSUE_ID) %in% c(1, 2, 3)))
})

