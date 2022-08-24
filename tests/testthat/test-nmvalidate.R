
n_tests <- length(nm_validate(nm, nm_spec))

# Check no failures
test_that("Works with no failures", {
  x = nm_validate(nm, nm_spec)
  expect_true(
  all(purrr::map_lgl(x, ~ .x$success)))
})

# Check failure on each test
test_that("Works with failure on test 1: no duplicates across id, tafd, and primary keys", {
  x <- nm %>%
    slice(rep(1,2)) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[1]]$success)
  expect_true(nrow(x[[1]]$error_content) == 2)
  expect_true(any(grepl(
    "No duplicates across id, tafd, and primary keys", #make lower case to match
    x[[1]]$description
  )))
})

test_that("Works with failure on test 1: within subject, no differing subject level covariates", {
  x <- nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4 & NUM == 68, 52)) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[2]]$success)
  expect_true(nrow(x[[2]]$error_content) == 2)
  expect_true(any(grepl(
    "no duplicate subject level covariates",
    x[[2]]$description
  )))
})

test_that("Works with failure on test 3: no na covariates", {
  x <- nm %>%
    mutate(AGEBL = replace(AGEBL, ID == 4, NA)) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[3]]$success)
  expect_true(nrow(x[[3]]$error_content) == 27)
  expect_true(any(grepl(
    "no na covariates",
    x[[3]]$description
  )))
})

test_that("Works with failure on test 4: similar continuous covariates across studies", {
  x <- nm %>%
    mutate(
      AGEBL = case_when(
        STUDYID == "STUDY-X" ~ AGEBL*0.5,
        TRUE ~ AGEBL
      )
    ) %>%
    nm_validate(nm_spec)
  expect_equal(
    sum(purrr::map_lgl(x, ~ .x$success)),
    length(x) - 1
  )
  expect_false(x[[4]]$success)
  expect_true(nrow(x[[4]]$error_content) == 2)
  expect_true(any(grepl(
    "similar continuous covariates across studies",
    x[[4]]$description
  )))
})

# test_that("Works with failure on test 4: similar continuous covariates across studies", {
#   x <- nm %>%
#     mutate(
#       AGEBL = case_when(
#         STUDYID == "STUDY-X" ~ AGEBL*0.7,
#         TRUE ~ AGEBL
#       )
#     ) %>%
#     nm_validate(nm_spec)
#   expect_equal(
#     sum(purrr::map_lgl(x, ~ .x$success)),
#     length(x) - 1
#   )
#   expect_false(x[[4]]$success)
#   expect_true(nrow(x[[4]]$error_content) == 2)
#   expect_true(any(grepl(
#     "similar continuous covariates across studies",
#     x[[4]]$description
#   )))
# })

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
    glue::glue("{n_tests} of {n_tests} checks PASSED"),
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
    glue::glue("1 of {n_tests} checks PASSED"),
    res
  )))
})







