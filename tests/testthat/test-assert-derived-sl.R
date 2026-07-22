valid_sl <- function() {
  list(
    sex = tibble::tibble(
      USUBJID = c("01", "02"),
      SEX = c(1, 2)
    ),
    blwt = tibble::tibble(
      USUBJID = c("01", "02"),
      BLWT = c(70, 80)
    )
  )
}

test_that("assert_derived_sl accepts a valid complete list", {
  sl <- valid_sl()

  expect_invisible(assert_derived_sl(sl))
  expect_identical(assert_derived_sl(sl), sl)
})

test_that("assert_derived_sl validates the outer list", {
  expect_error(assert_derived_sl(tibble::tibble(x = 1)), "named list")
  expect_error(assert_derived_sl(list()), "at least one")
  expect_error(assert_derived_sl(unname(valid_sl())), "unique, non-empty")
  expect_error(
    assert_derived_sl(stats::setNames(valid_sl(), c("sex", "SEX"))),
    "case-insensitive names"
  )
  expect_error(
    assert_derived_sl(valid_sl(), .subject_col = character()),
    "single non-empty string"
  )
})

test_that("assert_derived_sl requires data frames with one model variable", {
  sl <- list(
    sex = 1,
    covariates = tibble::tibble(
      USUBJID = "01",
      SEX = 1,
      RACE = 2
    ),
    wrong = tibble::tibble(
      USUBJID = "01",
      SEX = 1
    )
  )

  expect_error(
    assert_derived_sl(sl),
    regexp = "derived\\$sl\\$sex must be a data frame.*derived\\$sl\\$covariates must contain exactly.*derived\\$sl\\$wrong must be named for model variable SEX"
  )
})

test_that("assert_derived_sl requires its subject and unique column names", {
  duplicate_columns <- data.frame(
    USUBJID = "01",
    SEX = 1,
    SEX = 1,
    check.names = FALSE
  )

  expect_error(
    assert_derived_sl(list(sex = duplicate_columns)),
    "must have unique column names"
  )
  expect_error(
    assert_derived_sl(list(sex = tibble::tibble(ID = "01", SEX = 1))),
    "must contain USUBJID"
  )
})

test_that("assert_derived_sl requires populated unique subject identifiers", {
  sl <- list(
    sex = tibble::tibble(
      USUBJID = c("01", "01", "", NA_character_),
      SEX = c(1, 1, 2, 2)
    )
  )

  expect_error(
    assert_derived_sl(sl),
    regexp = "2 missing or blank USUBJID.*duplicated USUBJID value.*01"
  )
})

test_that("assert_derived_sl rejects grouped, empty, and list-column pieces", {
  sl <- list(
    sex = tibble::tibble(
      USUBJID = "01",
      SEX = list(1)
    ) %>%
      dplyr::group_by(.data$USUBJID),
    blwt = tibble::tibble(
      USUBJID = character(),
      BLWT = numeric()
    )
  )

  expect_error(
    assert_derived_sl(sl),
    regexp = "derived\\$sl\\$sex must be ungrouped.*list column.*SEX.*derived\\$sl\\$blwt must contain at least one row"
  )
})

test_that("assert_derived_sl supports another subject column", {
  sl <- list(age = tibble::tibble(SUBJID = c("01", "02"), AGE = c(40, 50)))

  expect_invisible(assert_derived_sl(sl, .subject_col = "SUBJID"))
})
