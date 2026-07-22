valid_tv <- function() {
  list(
    dosing = tibble::tibble(
      USUBJID = c("01", "02"),
      DATETIME = as.POSIXct(
        c("2024-01-01", "2024-01-02"),
        tz = "UTC"
      ),
      EVID = 1,
      AMT = c(100, 100)
    ),
    pc = tibble::tibble(
      USUBJID = c("01", "02"),
      DATETIME = as.POSIXct(
        c("2024-01-01", "2024-01-02 01:00"),
        tz = "UTC"
      ),
      EVID = 0,
      DV = c(10, 20)
    )
  )
}

test_that("assert_derived_tv accepts a valid complete list", {
  tv <- valid_tv()

  expect_invisible(assert_derived_tv(tv))
  expect_identical(assert_derived_tv(tv), tv)
})

test_that("assert_derived_tv allows keys repeated across elements", {
  tv <- valid_tv()
  tv$pc$DATETIME[[1]] <- tv$dosing$DATETIME[[1]]

  expect_invisible(assert_derived_tv(tv))
})

test_that("assert_derived_tv validates the outer list and key arguments", {
  expect_error(assert_derived_tv(tibble::tibble(x = 1)), "named list")
  expect_error(assert_derived_tv(list()), "at least one")
  expect_error(assert_derived_tv(unname(valid_tv())), "unique, non-empty")
  expect_error(
    assert_derived_tv(stats::setNames(valid_tv(), c("pc", "PC"))),
    "case-insensitive names"
  )
  expect_error(
    assert_derived_tv(valid_tv(), .subject_col = character()),
    "single non-empty string"
  )
  expect_error(
    assert_derived_tv(valid_tv(), .datetime_col = "USUBJID"),
    "must be different"
  )
})

test_that("assert_derived_tv requires data frames and key columns", {
  tv <- list(
    dosing = 1,
    pc = tibble::tibble(USUBJID = "01", DV = 10)
  )

  expect_error(
    assert_derived_tv(tv),
    regexp = "derived\\$tv\\$dosing must be a data frame.*derived\\$tv\\$pc must contain required column.*DATETIME"
  )
})

test_that("assert_derived_tv requires populated unique keys within elements", {
  tv <- list(
    pc = tibble::tibble(
      USUBJID = c("01", "01", "", NA_character_),
      DATETIME = c("2024-01-01", "2024-01-01", "", NA_character_),
      DV = c(10, 11, 12, 13)
    )
  )

  expect_error(
    assert_derived_tv(tv),
    regexp = "2 missing or blank USUBJID.*2 missing or blank DATETIME.*duplicated USUBJID \\+ DATETIME key.*01 @ 2024-01-01"
  )
})

test_that("assert_derived_tv rejects grouped, empty, and list-column pieces", {
  tv <- list(
    pc = tibble::tibble(
      USUBJID = "01",
      DATETIME = "2024-01-01",
      DV = list(10)
    ) %>%
      dplyr::group_by(.data$USUBJID),
    dosing = tibble::tibble(
      USUBJID = character(),
      DATETIME = character(),
      AMT = numeric()
    )
  )

  expect_error(
    assert_derived_tv(tv),
    regexp = "derived\\$tv\\$pc must be ungrouped.*list column.*DV.*derived\\$tv\\$dosing must contain at least one row"
  )
})

test_that("assert_derived_tv checks bind_rows type compatibility", {
  tv <- valid_tv()
  tv$pc$EVID <- as.character(tv$pc$EVID)

  expect_error(assert_derived_tv(tv), "cannot be combined with bind_rows")
})

test_that("assert_derived_tv supports alternate key columns", {
  tv <- list(
    pc = tibble::tibble(
      SUBJID = c("01", "02"),
      EVENT_TIME = c(1, 2),
      DV = c(10, 20)
    )
  )

  expect_invisible(
    assert_derived_tv(
      tv,
      .subject_col = "SUBJID",
      .datetime_col = "EVENT_TIME"
    )
  )
})
