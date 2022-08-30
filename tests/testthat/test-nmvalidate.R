
# Error on fail=FALSE -----------------------------------------------------


## No failure -------------------------------------------------------------

test_that("Works with no failures", {
  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE)
  expect_true(
    all(purrr::map_lgl(x, ~ .x$success)))
})


## Failure message for each test ------------------------------------------


### Test 1 ----------------------------------------------------------------

test_that("Works with critical failure on test 1: no duplicates across id, tafd, and primary keys", {
    nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 1, .err_row = 2, .desc = "No duplicates across: ID, TIME, EVID, DVID") %>%
    expect_true(.res[[.i]]$critical)
})


### Test 2 ----------------------------------------------------------------

test_that("Works with critical failure on test 2: within subject, no differing subject level covariates", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4 & NUM == 68, 52)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 2, .err_row = 2, .desc = "Duplicate baseline covariates") %>%
    expect_true(.res[[.i]]$critical)
})


### Test 3 ----------------------------------------------------------------

test_that("Works with critical failure on test 3: NA baseline covariates", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 3, .err_row = 27, .desc = "NA baseline covariates")
})


### Test 4 ----------------------------------------------------------------

test_that("Works with critical failure on test 4: NA time varying covariates", {
  nm %>%
    mutate(WT = replace(WT, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 4, .err_row = 27, .desc = "NA time varying covariates")
})


### Test 5 ----------------------------------------------------------------

test_that("Works with warning failure on test 5: Similar continuous baseline covariates across studies", {
  nm %>%
    mutate(
      AGEBL = case_when(
        STUDYID == "STUDY-X" ~ AGEBL*0.5,
        TRUE ~ AGEBL
      )
    ) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 5, .err_row = 2,
                       .desc = "Similar continuous baseline covariates across studies",
                       .err_type = "WARNING")
})


#Check pass for test 5
test_that("Works with barely pass on test 5: Similar continuous baseline covariates across studies", {
  x <- nm %>%
    mutate(
      AGEBL = case_when(
        STUDYID == "STUDY-X" ~ AGEBL*0.7,
        TRUE ~ AGEBL
      )
    ) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector()

  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x)
  )
  expect_true(x[[5]]$success)
  expect_false(x[[5]]$critical)
  expect_true(any(grepl_fixed(
    "Similar continuous baseline covariates across studies",
    x[[5]]$description
  )))
})


# Error on fail=TRUE ------------------------------------------------------

## No failure -------------------------------------------------------------

test_that("Works with no failures", {
  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = TRUE)
  expect_true(
    all(purrr::map_lgl(x, ~ .x$success)))
})

## Multiple failures ------------------------------------------------------
#potentially remove- same as critical test?
test_that("Works with multiple failures: nm_validate found critical issues in data", {
  nm_errors %>%
    nm_validate(.spec = nm_spec, .error_on_fail = TRUE) %>%
    as.vector() %>%
    expect_error(regexp ="nm_validate found critical issues in data")
})

## Critical failure -------------------------------------------------------

test_that("Works with critical failure on test 1: nm_validate found critical issues in data", {
  nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = TRUE) %>%
    as.vector() %>%
    expect_error(regexp ="nm_validate found critical issues in data")
})


## Warning failure --------------------------------------------------------
# copy eother one

# Print method ------------------------------------------------------------

## No failure -------------------------------------------------------------

test_that("Works with no critical failures, stop on failure: print method", {
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

test_that("Works with no failures, no stop on failure: print method", {
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

## Multiple failures ------------------------------------------------------


test_that("Works with some failures, stop on failure: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  nm_try <- try(nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = TRUE))
  # print and read result from temp file
  # capture.output(print(nm_try))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("{n_tests - 3} of {n_tests} checks PASSED"),
    res
  )))
  expect_true(inherits(nm_try, "try-error"))
}) #not working

test_that("Works with some failures, no stop on failure: print method", {
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



## Critical failure -------------------------------------------------------

test_that("Works with critical failure, stop on failure: print method", {

 # set up tempfile to sink output to
 .f <- tempfile()
 withr::defer(unlink(.f))
 withr::local_message_sink(.f)

 x <-  nm %>%
   slice(rep(1,2))

 nm_try <- try(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = TRUE))
 # print and read result from temp file
 # capture.output(print(nm_try))
 res <- readLines(.f)
 expect_true(any(grepl_fixed(
   glue::glue("{n_tests - 1} of {n_tests} checks PASSED"),
   res
 )))
 expect_true(inherits(nm_try, "try-error"))

}) # not working

test_that("Works with critical failure, no stop on failure: print method", {

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
    glue::glue("4 of {n_tests} checks PASSED (1 FAILURES)"),
    res
  )))
})


## Warning failure --------------------------------------------------------

test_that("Works with warning failure, stop on failure",{

  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  x <- nm %>%
    mutate(
      AGEBL = case_when(
        STUDYID == "STUDY-X" ~ AGEBL*0.5,
        TRUE ~ AGEBL
      )
    )

  # print and read result from temp file
  capture.output(print(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = TRUE)))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("4 of {n_tests} checks PASSED (1 WARNINGS)"),
    res
  )))
})

test_that("Works with warning failure, no stop on failure",{

  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  x <- nm %>%
    mutate(
      AGEBL = case_when(
        STUDYID == "STUDY-X" ~ AGEBL*0.5,
        TRUE ~ AGEBL
      )
    )

  # print and read result from temp file
  capture.output(print(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = FALSE)))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("4 of {n_tests} checks PASSED (1 WARNINGS)"),
    res
  )))
})


