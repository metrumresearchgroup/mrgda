test_df <- dplyr::tibble(
     AGE = c(45, 82, 73),
     WT = c(64, 23, 92),
     SC = c(1.02, 1.04, 1.98),
     SEX = c(1, 2, 1),
     RACE = c(1, 2, 3))

test_that("mutate_egfr appropriately calculates EGFR value", {
  .ex_egfr <- mutate_egfr(.df = test_df, .age = AGE, .wt = WT, .serum_creatinine = SC, .sex = SEX, .female_value = 1)
  expect_equal(.ex_egfr$EGFR[1], 69.13866)
  expect_equal(.ex_egfr$EGFR[2], 71.689694)
})

test_that("mutate_egfr reponds to change in specified female value", {
  .ex_egfr1 <- mutate_egfr(.df = test_df, .age = AGE, .wt = WT, .serum_creatinine = SC, .sex = SEX, .female_value = 1)
  .ex_egfr2 <- mutate_egfr(.df = test_df, .age = AGE, .wt = WT, .serum_creatinine = SC, .sex = SEX, .female_value = 2)
  expect_true(.ex_egfr1$EGFR[1] != .ex_egfr2$EGFR[1])
})
