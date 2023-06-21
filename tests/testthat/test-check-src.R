src_list <- mrgda::read_src_dir(system.file("example-sdtm", package = "mrgda")) %>% suppressMessages()
res <- check_src(src_list)

test_that("check_src ran tests across all relevant domains", {

  expect_true(all(names(res) %in% names(src_list)))
  expect_true(!"mrgda_labels" %in% names(res))

  expect_true(all(names(res$ae) %in% c("Duplicates", "MissingDatetimes")))

  expect_equal(res$ae$Duplicates$Result, "Pass")
  expect_equal(res$dm$Duplicates$Result, "Pass")
  expect_equal(res$lb$Duplicates$Result, "Fail")

  expect_equal(res$ae$MissingDatetimes$Result, "Pass")
  expect_equal(res$lb$MissingDatetimes$Result, "Pass")
  expect_equal(res$vs$MissingDatetimes$Result, "Pass")

  expect_true(nrow(res$lb$Duplicates$IssueRecords) == 21)
  expect_true(is.null(res$dm$MissingDatetimes))
})
