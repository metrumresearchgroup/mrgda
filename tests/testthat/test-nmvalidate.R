
# Error on fail=FALSE -----------------------------------------------------


## No failure -------------------------------------------------------------

test_that("nm_validate standard case: Works with no failures [NMV-VAL-001]", {
  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE)
  expect_true(
    all(purrr::map_lgl(x, ~ .x$success)))
})


## Failure message for each test ------------------------------------------


### Test 1 ----------------------------------------------------------------

test_that("nm_validate works with failure on test 1: No duplicates across id, tafd, and primary keys [NMV-VAL-002]", {
    nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 1, .err_row = 2, .desc = "No duplicates across: ID, TIME, EVID, DVID")
})


### Test 2 ----------------------------------------------------------------

test_that("nm_validate works with failure on test 2: Non-unique baseline covariates [NMV-VAL-003]", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4 & NUM == 68, 52)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 2, .err_row = 2, .desc = "Non-unique baseline covariates")
})


### Test 3 ----------------------------------------------------------------

test_that("nm_validate works with failure on test 2: Missing baseline covariates [NMV-VAL-003]", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 3, .err_row = 27, .desc = "Missing baseline covariates")
})


### Test 4 ----------------------------------------------------------------

test_that("nm_validate works with failure on test 4: Missing time varying covariates [NMV-VAL-004]", {
  nm %>%
    mutate(WT = replace(WT, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 4, .err_row = 27, .desc = "Missing time varying covariates")
})

# Error on fail=TRUE ------------------------------------------------------

## No failure -------------------------------------------------------------

test_that("nm_validate standard case: Works with no failures with error=TRUE [NMV-VAL-001]", {
  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = TRUE)
  expect_true(
    all(purrr::map_lgl(x, ~ .x$success)))
})

## Single failure -------------------------------------------------------

test_that("nm_validate works with failure on test 1:  Finds issues in data [NMV-VAL-002]", {
  nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = TRUE) %>%
    as.vector() %>%
    expect_error(regexp ="nm_validate found issues in data")
})



## Multiple failures ------------------------------------------------------

test_that("nm_validate works with multiple failures: Finds issues in data [NMV-VAL-005]", {
  nm_errors %>%
    nm_validate(.spec = nm_spec, .error_on_fail = TRUE) %>%
    as.vector() %>%
    expect_error(regexp ="nm_validate found issues in data")
})

# Print method ------------------------------------------------------------

## No failure -------------------------------------------------------------

test_that("nm_validate works with no failures, stop on failure: Print method [NMV-VAL-006]", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = TRUE)))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests} of {n_tests} checks PASSED"),
    res
  )))
})

test_that("nm_validate works with no failures, stop on failure: Print to temp file [NMV-VAL-006]", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  print(nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests} of {n_tests} checks PASSED"),
    res
  )))
})

## Single failure -------------------------------------------------------

test_that("nm_validate works with no failures, stop on failure: print failure [NMV-VAL-006]", {

  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  x <-  nm %>%
    slice(rep(1,2))

  nm_try <- try(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = TRUE))
  # print and read result from temp file
  capture.output(print(nm_try))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests - 1} of {n_tests} checks PASSED"),
    res
  )))
  expect_true(inherits(nm_try, "try-error"))

})

test_that("nm_validate works with no failures, stop on failure: Print specific failure [NMV-VAL-006]", {

  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  x <-  nm %>%
    slice(rep(1,2))

  # print and read result from temp file
  capture.output(print(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = FALSE)))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests-1} of {n_tests} checks PASSED (1 FAILURES)"),
    res
  )))
})





## Multiple failures ------------------------------------------------------


test_that("nm_validate works with multiple failures, stop on failure: Print method [NMV-VAL-007]", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  nm_try <- try(nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = TRUE))
  # print and read result from temp file
  capture.output(print(nm_try))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests - 3} of {n_tests} checks PASSED"),
    res
  )))
  expect_true(inherits(nm_try, "try-error"))
})

test_that("nm_validate works with multiple failures, stop on failure: Print failures [NMV-VAL-007]", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = FALSE)))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests - 3} of {n_tests} checks PASSED"),
    res
  )))
})


