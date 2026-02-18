empty_rows <- tibble::tibble(name = character(), value = character())

test_that("build_run_summary_lines includes header and all sections when both diffs exist", {
  standard_diffs <- tibble::tibble(
    name = "Rows",
    value = "1 added"
  )
  variable_diffs <- tibble::tibble(
    name = "WT",
    value = "3 diffs"
  )
  spec_diffs <- tibble::tibble(
    name = "Updated: WT",
    value = "short"
  )

  lines <- build_run_summary_lines(
    .data_standard_rows = standard_diffs,
    .data_variable_rows = variable_diffs,
    .spec_diff_rows = spec_diffs,
    .current_info = "local by tester at 2026-01-02 03:04:05",
    .baseline_info = "r120 by jsmith at 2026-01-15 10:30:45"
  )

  expect_true(any(grepl("Current:", lines, fixed = TRUE)))
  expect_true(any(grepl("Comparing against:", lines, fixed = TRUE)))
  expect_true(any(grepl("local by tester", lines, fixed = TRUE)))
  expect_true(any(grepl("r120 by jsmith", lines, fixed = TRUE)))
  expect_true(any(grepl("^Data changes:$", lines)))
  expect_true(any(grepl("^Variable changes:$", lines)))
  expect_true(any(grepl("^Spec changes:$", lines)))
  expect_true(any(grepl("Rows", lines, fixed = TRUE)))
  expect_true(any(grepl("WT", lines, fixed = TRUE)))
  expect_true(any(grepl("Updated: WT", lines, fixed = TRUE)))
})

test_that("build_run_summary_lines omits Variable changes section when no variable diffs", {
  standard_diffs <- tibble::tibble(
    name = "Subjects",
    value = "No change"
  )

  lines <- build_run_summary_lines(
    .data_standard_rows = standard_diffs,
    .data_variable_rows = empty_rows,
    .spec_diff_rows = empty_rows,
    .current_info = "local by tester at 2026-01-02 03:04:05",
    .baseline_info = "local by tester"
  )

  expect_true(any(grepl("^Data changes:$", lines)))
  expect_false(any(grepl("^Variable changes:$", lines)))
  expect_true(any(grepl("^Spec changes:$", lines)))
  expect_true(any(grepl("^No spec diffs detected\\.$", lines)))
})

test_that("build_run_summary_lines includes no-diff text when both inputs are empty", {
  lines <- build_run_summary_lines(
    .data_standard_rows = empty_rows,
    .data_variable_rows = empty_rows,
    .spec_diff_rows = empty_rows,
    .current_info = "local by tester at 2026-01-02 03:04:05",
    .baseline_info = "local by tester"
  )

  expect_true(any(grepl("Current:", lines, fixed = TRUE)))
  expect_true(any(grepl("Comparing against:", lines, fixed = TRUE)))
  expect_true(any(grepl("^Data changes:$", lines)))
  expect_true(any(grepl("^No data diffs detected\\.$", lines)))
  expect_true(any(grepl("^Spec changes:$", lines)))
  expect_true(any(grepl("^No spec diffs detected\\.$", lines)))
})

test_that("build_run_summary_lines includes baseline info", {
  lines <- build_run_summary_lines(
    .data_standard_rows = empty_rows,
    .data_variable_rows = empty_rows,
    .spec_diff_rows = empty_rows,
    .current_info = "local by tester at 2026-01-02 03:04:05",
    .baseline_info = "r120 by jsmith at 2026-01-15 10:30:45"
  )

  expect_true(any(grepl("r120 by jsmith", lines, fixed = TRUE)))
})
