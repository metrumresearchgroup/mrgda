.base_spec <- list(
  ID = list(short = "Subject ID", type = "character"),
  WT = list(short = "Weight", type = "numeric", unit = "kg")
)

.compare_spec <- list(
  WT = list(short = "Body Weight", type = "numeric", unit = "kg"),
  AGE = list(short = "Age", type = "numeric", unit = "years")
)

test_that("execute_spec_diffs returns empty diffs when specs are identical", {
  out <- execute_spec_diffs(.base_spec, .base_spec)
  expect_equal(nrow(out$diffs), 0)
})

test_that("execute_spec_diffs identifies added removed and updated spec content", {
  out <- execute_spec_diffs(.base_spec, .compare_spec)
  diffs <- out$diffs

  expect_true(any(diffs$name == "Spec Variables Added" & diffs$value == "AGE"))
  expect_true(any(diffs$name == "Spec Variables Removed" & diffs$value == "ID"))
  expect_true(any(diffs$name == "Spec Updated: WT" & grepl("short", diffs$value, fixed = TRUE)))
})

test_that("execute_spec_diffs handles NULL inputs", {
  out <- execute_spec_diffs(NULL, NULL)
  expect_equal(nrow(out$diffs), 0)
})
