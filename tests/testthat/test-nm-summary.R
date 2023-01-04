nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
dat_sum <- nm_summary(.data = nm, .spec = nm_spec)

# Baseline continuous covariates tests ------------------------------------

test_that("nm_summary base cont covariates: Short names are used in summary table [NMV-SUM-001]", {
  bcc <- dat_sum$Tables$Covariates$`Baseline continuous Covariates`$`_data`
  expect_equal(bcc$short[1], "Baseline age (years)")
  expect_equal(bcc$short[3], "Baseline weight (kg)")
})

test_that("nm_summary base cont covariates: Values are assigned appropriately [NMV-SUM-001]", {
  bcc <- dat_sum$Tables$Covariates$`Baseline continuous Covariates`$`_data`
  expect_true(bcc$MIN[1] < bcc$MEAN[1] & bcc$MEAN[1] < bcc$MAX[1])
  expect_true(bcc$MIN[3] < bcc$MEAN[3] & bcc$MEAN[3] < bcc$MAX[3])
})

test_that("nm_summary base cont covariates: All values are grouped by study [NMV-SUM-001]", {
  bcc <- dat_sum$Tables$Covariates$`Baseline continuous Covariates`$`_data` %>% dplyr::filter(short == "Baseline weight (kg)")
  expect_true(bcc$MIN[1] != bcc$MIN[2])
  expect_true(bcc$MEAN[1] != bcc$MEAN[2])
  expect_true(bcc$MAX[1] != bcc$MAX[2])
})

test_that("nm_summary baseline continuous covariates: All studies are included in table [NMV-SUM-001]", {
  bcc2 <- dat_sum$Tables$Covariates$`Baseline continuous Covariates`$`_data` %>% dplyr::distinct(STUDYID)
  expect_equal(nrow(bcc2), 2)
})

# Baseline categorical covariates -----------------------------------------

test_that("nm_summary baseline categorical covariates: Values fall within the expected range [NMV-SUM-002]", {
  bccat <- dat_sum$Tables$Covariates$`Baseline categorical Covariates`$`_data`
  expect_true(all(bccat$Percent < 100.1))
})

test_that("nm_summary baseline categorical covariates: Values within a group add up to 100 [NMV-SUM-002]", {
  bccat <- dat_sum$Tables$Covariates$`Baseline categorical Covariates`$`_data` %>% dplyr::filter(STUDYID == "STUDY-X" & short == "Race")
  expect_true(sum(bccat$Percent) == 100)
})

# Primary keys ------------------------------------------------------------

test_that("nm_summary primary keys: All EVID appear [NMV-SUM-003]", {
  bccat <- dat_sum$Tables$Miscellaneous$`Primary key summary`$`_data`
  expect_true(length(bccat$EVID) == 3)
})

# Figures -----------------------------------------------------------------

test_that("nm_summary outputs figures: Baseline covariates continuous [NMV-SUM-004]", {
  figsum <- dat_sum$Figures$Boxplots$`Baseline weight (kg)`$data
  expect_equal(nrow(figsum), 178)
  expect_equal(as.numeric(figsum[5, 4]), 105.1)
})

