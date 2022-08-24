
# Check no failures
test_that("Works with no failures", {
  x = nm_validate(nm, nm_spec)
  expect_true(
  all(purrr::map_lgl(x, ~ .x$success)))
})

# Check failure on test 1: no duplicates across id, tafd, and primary keys
test_that("Works with failure on test 1: no duplicates across id, tafd, and primary keys", {
  x <- nm %>%
    # rep 1 row
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 2
  )
  expect_false(x[[1]]$success)
  expect_true(nrow(x[[1]]$error_content) == 2)
  expect_true(any(grepl(
    "no duplicates across id, tafd, and primary keys",
    x[[1]]$description
  )))
})

# Check failure on test 2: no duplicate subject level covariates
test_that("Works with failure on test 1: no duplicate subject level covariates", {
  x <- nm %>%
   # make dup subj level covar
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 2
  )
  expect_false(x[[2]]$success)
  expect_true(nrow(x[[2]]$error_content) == 2)
  expect_true(any(grepl(
    "no duplicates across id, tafd, and primary keys",
    x[[2]]$description
  )))
})

# Check failure on test 3: no na covariates
test_that("Works with failure on test 3: no na covariates", {
  x <- nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4, NA)) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[3]]$success)
  expect_true(nrow(x[[3]]$error_content) == 27) # want 1- fix with x filter
  expect_true(any(grepl(
    "no na covariates",
    x[[3]]$description
  )))
})

# Check failure on test 4: similar continuous covariates across studies
test_that("Error content output", {
  x <- nm %>%
    mutate(mess up one column) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[3]]$success)
  expect_true(nrow(x[[3]]$error_content) == 1)
  #expect_true(anygrepl for description
})

# Check print method for no failures
test_that("Works with no failures: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  print(nm_validate(nm, nm_spec))
  res <- readLines(.f)
  expect_true(any(grepl(
    "4 of 4 checks PASSED",
    res
  )))
})

# Check print method for failures
test_that("Works with some failures: print method", {
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)

  # print and read result from temp file
  capture.output(print(nm_validate(nm_errors, nm_spec)))
  res <- readLines(.f)
  expect_true(any(grepl(
    "1 of 4 checks PASSED",
    res
  )))
})







