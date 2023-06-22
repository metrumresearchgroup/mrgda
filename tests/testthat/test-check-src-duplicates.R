src_list <- mrgda::read_src_dir(system.file("example-sdtm", package = "mrgda")) %>% suppressMessages()

test_that("check_src_duplicates works with multiple source domains", {

  dm_res <- check_src_duplicates(.domain_df = src_list$dm, .domain_name = "dm")
  expect_true(dm_res$Name == "dm duplicate record check")
  expect_true(all(dm_res$ColsCheck %in% c("STUDYID", "USUBJID")))
  expect_true(dm_res$Result == "Pass")

  ae_res <- check_src_duplicates(.domain_df = src_list$ae, .domain_name = "ae")
  expect_true(all(ae_res$ColsCheck %in% c("STUDYID", "AETERM", "AESTDTC", "USUBJID")))
  expect_true(ae_res$Result == "Pass")

  lb_res <- check_src_duplicates(.domain_df = src_list$lb, .domain_name = "lb")
  expect_true(all(lb_res$ColsCheck %in% c("STUDYID", "LBTESTCD", "LBDTC", "USUBJID")))
  expect_true(lb_res$Result == "Fail")
  expect_true(lb_res[["N fail (%)"]] == "21 (25.3)")

  vs_res <- check_src_duplicates(.domain_df = src_list$vs, .domain_name = "vs")
  expect_true(all(vs_res$ColsCheck %in% c("STUDYID", "VSTESTCD", "VSDTC", "USUBJID")))
  expect_true(vs_res$Result == "Pass")
})

df_test <-
  dplyr::tibble(
    USUBJID = c("a", "a", "b", "c", "d", "d"),
    TIME = c(1, 1, 1, 2, 3, 4),
    DVID = 1
  )

test_that("check_src_duplicates returns expected output", {
  test_res <- check_src_duplicates(.domain_df = df_test, .domain_name = "test_df", .cols_check = c("TIME", "DVID"), .subject_col = "USUBJID")
  expect_true(test_res$Result == "Fail")
  expect_true(all(test_res$ColsCheck %in% c("TIME", "DVID", "USUBJID")))
  expect_true(test_res[["N fail (%)"]] == "2 (33.33)")
})

