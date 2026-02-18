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
    .current_info = "by tester at 2026-01-02 03:04:05",
    .baseline_info = "by jsmith at 2026-01-15 10:30:45 (r120)"
  )

  expect_true(any(grepl("DIFF SUMMARY", lines, fixed = TRUE)))
  expect_true(any(grepl("Local:", lines, fixed = TRUE)))
  expect_true(any(grepl("Repository:", lines, fixed = TRUE)))
  expect_true(any(grepl("by tester", lines, fixed = TRUE)))
  expect_true(any(grepl("by jsmith", lines, fixed = TRUE)))
  expect_true(any(grepl("DATA CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("VARIABLE CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("SPEC CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("Rows", lines, fixed = TRUE)))
  expect_true(any(grepl("WT", lines, fixed = TRUE)))
  expect_true(any(grepl("Updated: WT", lines, fixed = TRUE)))
  # Should have === borders
  expect_true(any(grepl("^=+$", lines)))
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
    .current_info = "by tester at 2026-01-02 03:04:05",
    .baseline_info = "by tester"
  )

  expect_true(any(grepl("DATA CHANGES", lines, fixed = TRUE)))
  expect_false(any(grepl("VARIABLE CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("SPEC CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("No spec diffs found", lines, fixed = TRUE)))
})

test_that("build_run_summary_lines includes no-diff text when both inputs are empty", {
  lines <- build_run_summary_lines(
    .data_standard_rows = empty_rows,
    .data_variable_rows = empty_rows,
    .spec_diff_rows = empty_rows,
    .current_info = "by tester at 2026-01-02 03:04:05",
    .baseline_info = "by tester"
  )

  expect_true(any(grepl("Local:", lines, fixed = TRUE)))
  expect_true(any(grepl("Repository:", lines, fixed = TRUE)))
  expect_true(any(grepl("DATA CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("No data diffs found", lines, fixed = TRUE)))
  expect_true(any(grepl("SPEC CHANGES", lines, fixed = TRUE)))
  expect_true(any(grepl("No spec diffs found", lines, fixed = TRUE)))
})

test_that("build_run_summary_lines includes baseline info", {
  lines <- build_run_summary_lines(
    .data_standard_rows = empty_rows,
    .data_variable_rows = empty_rows,
    .spec_diff_rows = empty_rows,
    .current_info = "by tester at 2026-01-02 03:04:05",
    .baseline_info = "by jsmith at 2026-01-15 10:30:45 (r120)"
  )

  expect_true(any(grepl("by jsmith", lines, fixed = TRUE)))
})
