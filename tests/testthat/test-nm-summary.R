nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
dat_sum <- nm_summary(.data = nm, .spec = nm_spec, .type = "tables")

# Baseline continuous covariates tests ------------------------------------

test_that("nm_summary base cont covariates: Short names are used in summary table [NMV-SUM-001]", {
  bcc <- dat_sum$`1`
  expect_equal(bcc$short[1], "Baseline age (years)")
  expect_equal(bcc$short[5], "Baseline weight (kg)")
})

test_that("nm_summary base cont covariates: Values are assigned appropriately [NMV-SUM-001]", {
  bcc <- dat_sum$`1`
  expect_true(bcc$MIN[1] < bcc$MEAN[1] & bcc$MEAN[1] < bcc$MAX[1])
  expect_true(bcc$MIN[3] < bcc$MEAN[3] & bcc$MEAN[3] < bcc$MAX[3])
})

test_that("nm_summary base cont covariates: All values are grouped by study [NMV-SUM-001]", {
  bcc <- dat_sum$`1` %>% dplyr::filter(short == "Baseline weight (kg)")
  expect_true(bcc$MIN[1] != bcc$MIN[2])
  expect_true(bcc$MEAN[1] != bcc$MEAN[2])
  expect_true(bcc$MAX[1] != bcc$MAX[2])
})

test_that("nm_summary base cont covariates: All studies are included in table [NMV-SUM-001]", {
  bcc2 <- dat_sum$`1` %>% dplyr::distinct(STUDYID)
  expect_equal(nrow(bcc2), 2)
})

# Baseline categorical covariates -----------------------------------------

test_that("nm_summary base cat covariates: Values fall within the expected range [NMV-SUM-002]", {
  bccat <- dat_sum$`2`
  expect_true(all(bccat$Percent < 100.1))
})

test_that("nm_summary base cat covariates: Values within a group add up to 100 [NMV-SUM-002]", {
  bccat <- dat_sum$`2` %>% dplyr::filter(grepl("STUDY-X: Sex", BLCAT))
  expect_true(sum(bccat$Percent) == 100)
})

test_that("nm_summary base cat covariates: Correct caption is used [NMV-SUM-002]", {
  bccat <- dat_sum$`2`
  expect_equal(bccat$LT_CAP_TEXT[1], "Summary of baseline categorical covariates by study")
})

# Primary keys ------------------------------------------------------------

test_that("nm_summary primary keys: All EVID appear [NMV-SUM-003]", {
  bccat <- dat_sum$`3`
  expect_true(length(bccat$EVID) == 3)
})

test_that("nm_summary primary keys: Correct caption is used [NMV-SUM-003]", {
  bccat <- dat_sum$`3`
  expect_equal(bccat$LT_CAP_TEXT[1], "Summary of primary keys")
})

# Figures -----------------------------------------------------------------

test_that("nm_summary outputs figures: Baseline continuous covariates [NMV-SUM-004]", {
  figsum <- nm_summary(.data = nm, .spec = nm_spec, .type = "figures", .figure_prompt = FALSE)
  figdata <- figsum$`1`$data
  expect_equal(nrow(figdata), 178)
  expect_equal(as.numeric(figdata[5, 4]), 105.1)
})

test_that("nm_summary outputs figures: Baseline categorical covariates [NMV-SUM-004]", {
  figsum <- nm_summary(.data = nm, .spec = nm_spec, .type = "figures", .figure_prompt = FALSE)
  figdata <- figsum$`2`$data
  expect_equal(nrow(figdata), 178)
  expect_equal(as.numeric(figdata[177, 4]), 28.634435)
})

