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

  expect_true(any(diffs$name == "Added" & diffs$value == "AGE"))
  expect_true(any(diffs$name == "Removed" & diffs$value == "ID"))
  expect_true(any(diffs$name == "Updated: WT" & grepl("short", diffs$value, fixed = TRUE)))
})

test_that("execute_spec_diffs handles NULL inputs", {
  out <- execute_spec_diffs(NULL, NULL)
  expect_equal(nrow(out$diffs), 0)
})

test_that("execute_spec_diffs handles NULL base with non-NULL compare", {
  out <- execute_spec_diffs(NULL, .compare_spec)
  diffs <- out$diffs

  expect_true(any(diffs$name == "Added"))
  expect_true(grepl("WT", diffs$value[diffs$name == "Added"], fixed = TRUE))
  expect_true(grepl("AGE", diffs$value[diffs$name == "Added"], fixed = TRUE))
  expect_equal(sum(diffs$name == "Removed"), 0)
})

test_that("execute_spec_diffs handles non-NULL base with NULL compare", {
  out <- execute_spec_diffs(.base_spec, NULL)
  diffs <- out$diffs

  expect_true(any(diffs$name == "Removed"))
  expect_true(grepl("ID", diffs$value[diffs$name == "Removed"], fixed = TRUE))
  expect_true(grepl("WT", diffs$value[diffs$name == "Removed"], fixed = TRUE))
  expect_equal(sum(diffs$name == "Added"), 0)
})

test_that("execute_spec_diffs reports only additions when all variables are new", {
  out <- execute_spec_diffs(list(), .compare_spec)
  diffs <- out$diffs

  expect_equal(nrow(diffs), 1)
  expect_equal(diffs$name, "Added")
  expect_equal(diffs$value, "WT, AGE")
})

test_that("execute_spec_diffs reports only removals when all variables are dropped", {
  out <- execute_spec_diffs(.base_spec, list())
  diffs <- out$diffs

  expect_equal(nrow(diffs), 1)
  expect_equal(diffs$name, "Removed")
  expect_equal(diffs$value, "ID, WT")
})

test_that("execute_spec_diffs detects multiple changed fields on one variable", {
  base <- list(WT = list(short = "Weight", type = "numeric", unit = "kg"))
  compare <- list(WT = list(short = "Body Weight", type = "character", unit = "lbs"))
  out <- execute_spec_diffs(base, compare)
  diffs <- out$diffs

  expect_equal(nrow(diffs), 1)
  expect_equal(diffs$name, "Updated: WT")
  expect_true(grepl("short", diffs$value, fixed = TRUE))
  expect_true(grepl("type", diffs$value, fixed = TRUE))
  expect_true(grepl("unit", diffs$value, fixed = TRUE))
})

test_that("execute_spec_diffs detects changes in values and decode fields", {
  base <- list(SEX = list(short = "Sex", type = "character", values = c(0, 1), decode = c("M", "F")))
  compare <- list(SEX = list(short = "Sex", type = "character", values = c(0, 1, 2), decode = c("M", "F", "U")))
  out <- execute_spec_diffs(base, compare)
  diffs <- out$diffs

  expect_equal(nrow(diffs), 1)
  expect_equal(diffs$name, "Updated: SEX")
  expect_true(grepl("values", diffs$value, fixed = TRUE))
  expect_true(grepl("decode", diffs$value, fixed = TRUE))
  expect_false(grepl("short", diffs$value, fixed = TRUE))
})

test_that("execute_spec_diffs ignores fields not in the tracked set", {
  base <- list(WT = list(short = "Weight", custom_field = "abc"))
  compare <- list(WT = list(short = "Weight", custom_field = "xyz"))
  out <- execute_spec_diffs(base, compare)

  expect_equal(nrow(out$diffs), 0)
})

test_that("execute_spec_diffs reports multiple added and removed variables", {
  base <- list(A = list(short = "a"), B = list(short = "b"))
  compare <- list(C = list(short = "c"), D = list(short = "d"))
  out <- execute_spec_diffs(base, compare)
  diffs <- out$diffs

  expect_equal(diffs$value[diffs$name == "Added"], "C, D")
  expect_equal(diffs$value[diffs$name == "Removed"], "A, B")
})
