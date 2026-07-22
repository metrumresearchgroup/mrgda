valid_sl <- function() {
  list(
    sex = tibble::tibble(USUBJID = c("01", "02"), SEX = c(1, 2)),
    blwt = tibble::tibble(USUBJID = c("01", "02"), BLWT = c(70, 80))
  )
}

test_that("assert_derived_sl accepts a valid list", {
  sl <- valid_sl()

  expect_invisible(assert_derived_sl(sl))
  expect_identical(assert_derived_sl(sl), sl)
})

test_that("assert_derived_sl requires a named nonempty list", {
  expect_error(assert_derived_sl(tibble::tibble(x = 1)), "nonempty named list")
  expect_error(assert_derived_sl(list()), "nonempty named list")
  expect_error(assert_derived_sl(unname(valid_sl())), "unique, nonempty names")
  expect_error(
    assert_derived_sl(stats::setNames(valid_sl(), c("sex", "SEX"))),
    "unique, nonempty names"
  )
})

test_that("assert_derived_sl requires data frames", {
  expect_error(
    assert_derived_sl(list(sex = 1)),
    "derived\\$sl\\$sex must be a data frame"
  )
})

test_that("assert_derived_sl requires ungrouped nonempty data", {
  grouped <- valid_sl()$sex %>% dplyr::group_by(.data$USUBJID)
  empty <- tibble::tibble(USUBJID = character(), SEX = numeric())

  expect_error(assert_derived_sl(list(sex = grouped)), "must be ungrouped")
  expect_error(assert_derived_sl(list(sex = empty)), "at least one row")
})

test_that("assert_derived_sl requires simple unique columns", {
  duplicate_columns <- data.frame(
    USUBJID = "01",
    SEX = 1,
    SEX = 1,
    check.names = FALSE
  )
  list_column <- tibble::tibble(USUBJID = "01", SEX = list(1))

  expect_error(
    assert_derived_sl(list(sex = duplicate_columns)),
    "unique column names"
  )
  expect_error(assert_derived_sl(list(sex = list_column)), "list columns")
})

test_that("assert_derived_sl requires USUBJID and one matching variable", {
  no_subject <- tibble::tibble(ID = "01", SEX = 1)
  two_variables <- tibble::tibble(USUBJID = "01", SEX = 1, RACE = 2)
  wrong_name <- tibble::tibble(USUBJID = "01", SEX = 1)

  expect_error(assert_derived_sl(list(sex = no_subject)), "contain USUBJID")
  expect_error(
    assert_derived_sl(list(covariates = two_variables)),
    "one model variable"
  )
  expect_error(
    assert_derived_sl(list(wrong = wrong_name)),
    "named for model variable SEX"
  )
})

test_that("assert_derived_sl requires populated unique subjects", {
  blank_subject <- tibble::tibble(USUBJID = c("01", ""), SEX = c(1, 2))
  duplicate_subject <- tibble::tibble(USUBJID = c("01", "01"), SEX = c(1, 2))

  expect_error(
    assert_derived_sl(list(sex = blank_subject)),
    "missing or blank USUBJID"
  )
  expect_error(
    assert_derived_sl(list(sex = duplicate_subject)),
    "duplicated USUBJID"
  )
})
