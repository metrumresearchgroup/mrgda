# Load in spec and data
nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
nm_errors <- readr::read_csv(system.file("derived", "pk-errors.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

nm_spec_noflags <- yspec::ys_load(system.file("derived", "pk-noflags.yml", package = "mrgda"))
nm_spec_parflags <- yspec::ys_load(system.file("derived", "pk-partial_flags.yml", package = "mrgda"))


# returns appropriate debug code when issue detected ----------------------
test_that("nm_validate debug code: Returns usable debug code [NMV-VAL-001]", {

  x = nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = FALSE)
  # Test 1
  expect_equal(nrow(rlang::parse_expr(x$`No duplicate records`$debug) %>% rlang::eval_tidy()), 1)
  # Test 2
  expect_equal(nrow(rlang::parse_expr(x$`Non-unique baseline covariates`$debug) %>% rlang::eval_tidy()), 2)
  # Test 3
  expect_equal(nrow(rlang::parse_expr(x$`No missing covariates`$debug) %>% rlang::eval_tidy()), 1)
})


# duplicates across id, time, primary_keys fails --------------------------
test_that("nm_validate catches duplicates across id, time, primary_keys [NMV-VAL-002]", {
  nm_dups <- nm_errors
  nm_dups$EVID[1:10] = 0
  nm_dups$DVID[1:10] = 1
  nm_dups$TIME[1:10] = 100
  x = nm_validate(.data = nm_dups, .spec = nm_spec, .error_on_fail = FALSE)
  n_val_df <- rlang::parse_expr(x$`No duplicate records`$debug) %>% rlang::eval_tidy()
  n_val <- n_val_df$n

  expect_equal(n_val, 10)
})


# non-unique baseline covariates -------------------------------------------
test_that("nm_validate catches non-unique baseline covariates [NMV-VAL-003]", {
  nm_nonuni <- nm_errors
  nm_nonuni$WTBL[4] = 79.5
  nm_nonuni$WTBL[5] = 80.4
  nm_nonuni$WTBL[6] = 90.5
  x = nm_validate(.data = nm_nonuni, .spec = nm_spec, .error_on_fail = FALSE)
  n_val_df <- rlang::parse_expr(x$`Non-unique baseline covariates`$debug) %>% rlang::eval_tidy()

  expect_equal(nrow(n_val_df), 5)
})


# missing covariates -------------------------------------------------------
test_that("nm_validate catches missing covariates [NMV-VAL-004]", {
  nm_missc <- nm_errors
  nm_missc$WTBL[4] = NA_real_
  nm_missc$WT[5] = NA_real_
  nm_missc$SEX[6] = NA_real_
  x = nm_validate(.data = nm_missc, .spec = nm_spec, .error_on_fail = FALSE)
  n_val_df <- rlang::parse_expr(x$`No missing covariates`$debug) %>% rlang::eval_tidy()

  expect_equal(nrow(n_val_df), 4)
  expect_equal(n_val_df$WT[1], NA_real_)
  expect_equal(n_val_df$WTBL[2], NA_real_)
  expect_equal(n_val_df$SEX[4], NA_real_)
})


# output all failures ---------------------------------------------------
test_that("nm_validate prints multiple failures [NMV-VAL-005]", {
  x = nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = FALSE)

  expect_false(x$`No duplicate records`$success)
  expect_false(x$`Non-unique baseline covariates`$success)
  expect_false(x$`No missing covariates`$success)

  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE)

  expect_true(x$`No duplicate records`$success)
  expect_true(x$`Non-unique baseline covariates`$success)
  expect_true(x$`No missing covariates`$success)
})


# output all failures ---------------------------------------------------
test_that("nm_validate works when arguments are provided out of order [NMV-VAL-006]", {
  x = nm_validate(.spec = nm_spec, .data = nm_errors, .error_on_fail = FALSE)

  expect_true(grepl("nm_errors", x$`No duplicate records`$debug))
  expect_true(grepl("nm_errors", x$`Non-unique baseline covariates`$debug))
  expect_true(grepl("nm_errors", x$`No missing covariates`$debug))
})


# output prints appropriate test names ------------------------------------
test_that("nm_validate prints correct test names when only 1 failure  [NMV-VAL-007]", {
  nm_1e <- nm
  nm_1e$WTBL[1] = NA_real_
  x = nm_validate(.spec = nm_spec, .data = nm_1e, .error_on_fail = FALSE)

  expect_true(names(x)[3] == "No missing covariates")
})

# output catches NA, Inf and -Inf for non-finite TIME --------------------
test_that("nm_validate catches all cases of non-finite TIME [NMV-VAL-008]", {
  nm_1e <- nm_errors
  nm_1e$TIME[1] = NA
  nm_1e$TIME[2] = Inf
  nm_1e$TIME[3] = -Inf

  x = nm_validate(.spec = nm_spec, .data = nm_1e, .error_on_fail = FALSE)

  expect_true(nrow(rlang::parse_expr(x$`Non-finite TIME values`$debug) %>% rlang::eval_tidy()) == 3)
})

# output catches when MDV incorrectly set for NA DV --------------------
test_that("nm_validate checks if MDV is set to 1 for all rows with NA DV [NMV-VAL-009]", {
  nm_1e <- nm
  nm_1e$DV[1] = NA_real_
  nm_1e$MDV[1] = 0

  x = nm_validate(.spec = nm_spec, .data = nm_1e, .error_on_fail = FALSE)
  expect_true(nrow(rlang::parse_expr(x$`MDV not set to 1 when DV is NA or 0`$debug) %>% rlang::eval_tidy()) == 1)

  x = nm_validate(.spec = nm_spec, .data = nm, .error_on_fail = FALSE)
  expect_true(nrow(rlang::parse_expr(x$`MDV not set to 1 when DV is NA or 0`$debug) %>% rlang::eval_tidy()) == 0)

  nm_2e <- nm %>% dplyr::select(-MDV)
  x = nm_validate(.spec = nm_spec, .data = nm_2e, .error_on_fail = FALSE)
  expect_true(is.na(x$`MDV not set to 1 when DV is NA or 0`$success))
})

test_that("nm_validate checks if MDV is set to 1 for all rows with 0 DV [NMV-VAL-009]", {
  nm_2e <- nm
  nm_2e$DV[1] = 0
  nm_2e$MDV[1] = 0

  x = nm_validate(.spec = nm_spec, .data = nm_2e, .error_on_fail = FALSE)
  expect_true(nrow(rlang::parse_expr(x$`MDV not set to 1 when DV is NA or 0`$debug) %>% rlang::eval_tidy()) == 1)

})

# output ensures only unique NUM values in dataset --------------------
test_that("nm_validate checks if all NUM are unique [NMV-VAL-010]", {
  nm_num <- nm

  x = nm_validate(.spec = nm_spec, .data = nm_num, .error_on_fail = FALSE)
  expect_true(nrow(rlang::parse_expr(x$`All NUM values are unique`$debug) %>% rlang::eval_tidy()) == 0)

  nm_num$NUM[2] <- 1

  x = nm_validate(.spec = nm_spec, .data = nm_num, .error_on_fail = FALSE)
  expect_false(x$`All NUM values are unique`$success)
})

# output ensures all dosing rows have AMT = RATE*DUR --------------------
test_that("nm_validate checks if AMT always equals RATE times DUR [NMV-VAL-011]", {
  x = nm_validate(.spec = nm_spec, .data = nm, .error_on_fail = FALSE)
  expect_true(x$`All dosing AMT values are equivalent to RATE * DUR`$success)

  nm_amt <- nm
  nm_amt$AMT[2] = 1
  x = nm_validate(.spec = nm_spec, .data = nm_amt, .error_on_fail = FALSE)

  expect_equal(
    nrow(rlang::parse_expr(x$`All dosing AMT values are equivalent to RATE * DUR`$debug) %>% rlang::eval_tidy()),
    1)

})
