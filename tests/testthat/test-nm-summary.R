nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "nmvalidate"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "nmvalidate"), na = ".", show_col_types = FALSE)


# Baseline continuous covariates tests ------------------------------------

test_that("Short names are used in summary table", {
  bcc <- dat_sum$`1`
  expect_equal(bcc$short[1], "Baseline age (years)")
  expect_equal(bcc$short[5], "Baseline weight (kg)")
})

test_that("Values are assigned appropriately", {
  bcc <- dat_sum$`1`
  expect_true(bcc$MIN[1] < bcc$MEAN[1] & bcc$MEAN[1] < bcc$MAX[1])
  expect_true(bcc$MIN[3] < bcc$MEAN[3] & bcc$MEAN[3] < bcc$MAX[3])
})

test_that("All values are grouped by study", {
  bcc <- dat_sum$`1` %>% dplyr::filter(short == "Baseline weight (kg)")
  expect_true(bcc$MIN[1] != bcc$MIN[2])
  expect_true(bcc$MEAN[1] != bcc$MEAN[2])
  expect_true(bcc$MAX[1] != bcc$MAX[2])
})

test_that("All studies are included in table", {
  bcc2 <- dat_sum$`1` %>% dplyr::distinct(STUDYID)
  expect_equal(nrow(bcc2), 2)
})

# Baseline categorical covariates -----------------------------------------

test_that("Values fall within the expected range", {
  bccat <- dat_sum$`2`
  expect_true(all(bccat$n < 100.1))
})

test_that("Values within a group add up to 100", {
  bccat <- dat_sum$`2` %>% dplyr::filter(grepl("STUDY-X: Sex", BLCAT))
  expect_true(sum(bccat$n) == 100)
})

test_that("Correct caption is used", {
  bccat <- dat_sum$`2`
  expect_equal(bccat$LT_CAP_TEXT[1], "Summary of baseline categorical covariates by study")
})

# Primary keys ------------------------------------------------------------

test_that("All EVID appear", {
  bccat <- dat_sum$`3`
  expect_true(length(bccat$EVID) == 3)
})
