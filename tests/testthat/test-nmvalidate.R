# NEW CHECK (is one old check now split into two)
# NEW MESSAGES: warnings now included
# NEW FUNCTIONALITY: function throws an error
## ALWAYS EXPLIICITLY SET .error_on_fail
## SET .error_on_fail=TRUE have one test that errors with a critical
## SET .error_on_fail=TRUE have one test that does not error without a critical (BUT it has a failure)
## SET .error_on_fail=TRUE have one test that does not error without a critical (AND HAS NO FAILURES)
n_tests <- length(nm_validate(nm, nm_spec))

# Check no failures
test_that("Works with no failures", {
  x = nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE)
  expect_true(
    all(purrr::map_lgl(x, ~ .x$success)))
})

# Check failure on each test
test_that("Works with critical failure on test 1: no duplicates across id, tafd, and primary keys", {
    nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 1, .err_row = 2, .desc = "No duplicates across: ID, TIME, EVID, DVID") %>%
    expect_true(.res[[.i]]$critical)
})


test_that("Works with critical failure on test 2: within subject, no differing subject level covariates", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4 & NUM == 68, 52)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 2, .err_row = 2, .desc = "Duplicate baseline covariates") %>%
    expect_true(.res[[.i]]$critical)
})

test_that("Works with critical failure on test 3: NA baseline covariates", {
  nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 3, .err_row = 27, .desc = "NA baseline covariates")
})

test_that("Works with critical failure on test 4: NA time varying covariates", {
  nm %>%
    mutate(WT = replace(WT, ID == 4, NA)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
    as.vector() %>%
    check_single_error(.i = 4, .err_row = 27, .desc = "NA time varying covariates")
})

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
    check_single_error(.i = 5, .err_row = 2, .desc = "Similar continuous baseline covariates across studies", .err_type = "WARNING")
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
  expect_true(any(grepl(
    "Similar continuous baseline covariates across studies",
    x[[5]]$description
  )))
})


# Check print method and error fail functionality

# Check print method for multiple failures, stop on failure
test_that("Works with some failures, stop on failure: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = TRUE)))
  res <- readLines(.f)
  # expect_true(any(grepl(
  #   glue::glue("2 of {n_tests} checks PASSED"), #or try "Error: nm_validate found critical issues in data"
  #   res
  # )))
  expect_error("nm_validate found critical issues in data", res)
}) #not working

# Check print method for multiple failures, no stop on failure
test_that("Works with some failures, no stop on failure: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = FALSE)))
  res <- readLines(.f)
  expect_true(any(grepl(
    glue::glue("2 of {n_tests} checks PASSED"),
    res
  )))
}) #not working

# Check print method for 1 critical failure, stop on failure
test_that("Works with critical failure, stop on failure: break code", {
 error_fail_true <-  nm %>%
    slice(rep(1,2)) %>%
    nm_validate(.spec = nm_spec, .error_on_fail = TRUE) %>%
    as.vector() %>%
   expect_error("nm_validate found critical issues in data") # this part works

 x <-  nm %>%
       slice(rep(1,2))
 # set up tempfile to sink output to
 .f <- tempfile()
 withr::defer(unlink(.f))
 withr::local_message_sink(.f)

 # print and read result from temp file
 capture.output(print(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = TRUE)))
 res <- readLines(.f)
 expect_true(any(grepl(
   glue::glue("4 of {n_tests} checks PASSED (1 FAILURES)"),
   res
 )))
}) # not working

# Check print method for 1 critical failure, no stop on failure
test_that("Works with critical failure, no stop on failure: does not break code", {
  # error_fail_true <-  nm %>%
  #   slice(rep(1,2)) %>%
  #   nm_validate(.spec = nm_spec, .error_on_fail = FALSE) %>%
  #   as.vector() %>%
  #   expect_message("4 of 5 checks PASSED (1 FAILURES)")

  x <-  nm %>%
    slice(rep(1,2))
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = x, .spec = nm_spec, .error_on_fail = FALSE)))
  res <- readLines(.f)
  expect_true(any(grepl(
    glue::glue("4 of {n_tests} checks PASSED (1 FAILURES)"), #4 of {n_tests} checks PASSED (1 FAILURES)
    res
  )))
}) # not working

# Check print method for warning failure, stop on failure
test_that("Works with warning failure: does not break code",{
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
  expect_true(any(grepl(
    glue::glue("4 of {n_tests} checks PASSED (1 WARNINGS)"),
    res
  )))
}) #not working

# Check print method for warning failure, no stop on failure
    ## add once figure out issues with others

# Check print method for no failure, stop on failure
test_that("Works with no critical failures, stop on failure: does not break code", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = TRUE)))
  res <- readLines(.f)
  expect_true(any(grepl(
    glue::glue("2 of {n_tests} checks PASSED"),
    res
  )))
})

# Check print method for no failures, no stop on failure
test_that("Works with no failures, no stop on failure: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  print(nm_validate(.data = nm, .spec = nm_spec, .error_on_fail = FALSE))
  res <- readLines(.f)
  expect_true(any(grepl(
    glue::glue("{n_tests} of {n_tests} checks PASSED"),
    res
  )))
})


