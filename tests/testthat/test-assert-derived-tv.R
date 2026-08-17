valid_tv <- function() {
  list(
    dosing = tibble::tibble(
      USUBJID = c("01", "02"),
      DATETIME = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"),
      EVID = 1,
      AMT = c(100, 100)
    ),
    pc = tibble::tibble(
      USUBJID = c("01", "02"),
      DATETIME = as.POSIXct(
        c("2024-01-01 01:00", "2024-01-02 01:00"),
        tz = "UTC"
      ),
      EVID = 0,
      DV = c(10, 20)
    )
  )
}

test_that("assert_derived_tv accepts a valid list", {
  tv <- valid_tv()

  expect_invisible(assert_derived_tv(tv))
  expect_identical(assert_derived_tv(tv), tv)
})

test_that("assert_derived_tv allows keys repeated across elements", {
  tv <- valid_tv()
  tv$pc$DATETIME[[1]] <- tv$dosing$DATETIME[[1]]

  expect_invisible(assert_derived_tv(tv))
})

test_that("assert_derived_tv requires a named nonempty list", {
  expect_error(assert_derived_tv(tibble::tibble(x = 1)), "nonempty named list")
  expect_error(assert_derived_tv(list()), "nonempty named list")
  expect_error(assert_derived_tv(unname(valid_tv())), "unique, nonempty names")
  expect_error(
    assert_derived_tv(stats::setNames(valid_tv(), c("pc", "PC"))),
    "unique, nonempty names"
  )
})

test_that("assert_derived_tv requires data frames", {
  expect_error(
    assert_derived_tv(list(dosing = 1)),
    "derived\\$tv\\$dosing must be a data frame"
  )
})

test_that("assert_derived_tv requires ungrouped nonempty data", {
  grouped <- valid_tv()$pc %>% dplyr::group_by(.data$USUBJID)
  empty <- tibble::tibble(
    USUBJID = character(),
    DATETIME = as.POSIXct(character())
  )

  expect_error(assert_derived_tv(list(pc = grouped)), "must be ungrouped")
  expect_error(assert_derived_tv(list(pc = empty)), "at least one row")
})

test_that("assert_derived_tv requires simple unique columns", {
  duplicate_columns <- data.frame(
    USUBJID = "01",
    DATETIME = "2024-01-01",
    DV = 1,
    DV = 1,
    check.names = FALSE
  )
  list_column <- tibble::tibble(
    USUBJID = "01",
    DATETIME = "2024-01-01",
    DV = list(1)
  )

  expect_error(
    assert_derived_tv(list(pc = duplicate_columns)),
    "unique column names"
  )
  expect_error(assert_derived_tv(list(pc = list_column)), "list columns")
})

test_that("assert_derived_tv requires USUBJID and DATETIME", {
  no_datetime <- tibble::tibble(USUBJID = "01", DV = 1)

  expect_error(
    assert_derived_tv(list(pc = no_datetime)),
    "contain USUBJID and DATETIME"
  )
})

test_that("assert_derived_tv requires populated unique keys", {
  blank_subject <- tibble::tibble(
    USUBJID = "",
    DATETIME = "2024-01-01"
  )
  blank_datetime <- tibble::tibble(
    USUBJID = "01",
    DATETIME = ""
  )
  duplicate_key <- tibble::tibble(
    USUBJID = c("01", "01"),
    DATETIME = c("2024-01-01", "2024-01-01"),
    DV = c(1, 2)
  )

  expect_error(
    assert_derived_tv(list(pc = blank_subject)),
    "missing or blank USUBJID"
  )
  expect_error(
    assert_derived_tv(list(pc = blank_datetime)),
    "missing or blank DATETIME"
  )
  expect_error(
    assert_derived_tv(list(pc = duplicate_key)),
    "duplicated USUBJID \\+ DATETIME"
  )
})

test_that("assert_derived_tv checks bind_rows compatibility", {
  tv <- valid_tv()
  tv$pc$EVID <- as.character(tv$pc$EVID)

  expect_error(assert_derived_tv(tv), "Can't combine")
})
