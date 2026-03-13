nm_spec_s <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm_s <- readr::read_csv(
  system.file("derived", "pk.csv", package = "mrgda"),
  na = ".", show_col_types = FALSE
)

svn_is_available <- function() {
  result <- tryCatch(
    system("svn --version", ignore.stdout = TRUE, ignore.stderr = TRUE),
    error = function(e) 1
  )
  identical(result, 0L)
}

# ── Scenario 1: First run (brand new) ────────────────────────────────────────

test_that("first run creates csv, xpt, define, spec-list but no diff-summary", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    meta <- file.path(getwd(), "pk")
    expect_true(file.exists(csv_path))
    expect_true(file.exists(file.path(meta, "pk.xpt")))
    expect_true(file.exists(file.path(meta, "spec-list.yml")))
    define_files <- list.files(meta, pattern = "^define", full.names = TRUE)
    skip_if(length(define_files) == 0, "define document was not rendered")
    expect_true(file.exists(define_files[1]))
    expect_false(file.exists(file.path(meta, "diff-summary.txt")))
  })
})

test_that("first run with .return_base_compare returns NULL base_df", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    result <- write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE, .return_base_compare = TRUE
    ) %>% suppressMessages()

    expect_true(is.list(result))
    expect_null(result$base_df)
    expect_s3_class(result$compare_df, "data.frame")
    expect_false(result$base_from_svn)
  })
})

test_that("first run outputs message listing csv, xpt, define", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    expect_message(
      write_derived(
        .data = nm_s, .spec = nm_spec_s, .file = csv_path,
        .compare_from_svn = FALSE
      ),
      "csv, xpt, define"
    )
  })
})

# ── Scenario 2: Re-run with no changes ───────────────────────────────────────

test_that("unchanged re-run emits 'No changes since last run' message", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    expect_message(
      write_derived(
        .data = nm_s, .spec = nm_spec_s, .file = csv_path,
        .compare_from_svn = FALSE
      ),
      "No changes since last run"
    )
  })
})

test_that("unchanged re-run lists only csv in outputs message", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    msgs <- character()
    withCallingHandlers(
      write_derived(
        .data = nm_s, .spec = nm_spec_s, .file = csv_path,
        .compare_from_svn = FALSE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )

    output_msg <- grep("Outputs written", msgs, value = TRUE)
    expect_length(output_msg, 1)
    expect_false(grepl("xpt", output_msg))
    expect_false(grepl("define", output_msg))
  })
})

test_that("unchanged re-run references existing diff-summary.txt when present", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Create a diff-summary.txt by changing data
    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1
    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    meta <- file.path(getwd(), "pk")
    expect_true(file.exists(file.path(meta, "diff-summary.txt")))

    # Re-run with same data — should reference existing diff-summary
    expect_message(
      write_derived(
        .data = nm2, .spec = nm_spec_s, .file = csv_path,
        .compare_from_svn = FALSE
      ),
      "diff-summary"
    )
  })
})

# ── Scenario 3: Both data and spec changed ───────────────────────────────────

test_that("diff-summary contains both data and spec changes when both change", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1
    nm_spec2 <- nm_spec_s
    nm_spec2$WT$short <- "Modified Weight Label"

    write_derived(
      .data = nm2, .spec = nm_spec2, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    # Data section should show actual changes (not "No data diffs found")
    expect_true(any(grepl("DATA CHANGES", diff_lines, fixed = TRUE)))
    expect_false(any(grepl("No data diffs found", diff_lines, fixed = TRUE)))
    # Spec section should show actual changes (not "No spec diffs found")
    expect_true(any(grepl("SPEC CHANGES", diff_lines, fixed = TRUE)))
    expect_false(any(grepl("No spec diffs found", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Updated: WT", diff_lines, fixed = TRUE)))
  })
})

test_that("spec change alone triggers xpt and define regeneration", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    meta <- file.path(getwd(), "pk")
    xpt_time <- file.mtime(file.path(meta, "pk.xpt"))
    Sys.sleep(1)

    nm_spec2 <- nm_spec_s
    nm_spec2$WT$short <- "Modified Weight Label"

    write_derived(
      .data = nm_s, .spec = nm_spec2, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    expect_gt(file.mtime(file.path(meta, "pk.xpt")), xpt_time)
  })
})

# ── Scenario 4: .execute_diffs = FALSE ────────────────────────────────────────

test_that(".execute_diffs = FALSE skips diff-summary even when content changes", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE, .execute_diffs = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_false(file.exists(diffs_path))
  })
})

test_that(".execute_diffs = FALSE still regenerates xpt when content changes", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    meta <- file.path(getwd(), "pk")
    xpt_time <- file.mtime(file.path(meta, "pk.xpt"))
    Sys.sleep(1)

    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE, .execute_diffs = FALSE
    ) %>% suppressMessages()

    expect_gt(file.mtime(file.path(meta, "pk.xpt")), xpt_time)
  })
})

# ── Scenario 5: .return_base_compare ──────────────────────────────────────────

test_that(".return_base_compare returns base and compare after data change", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1

    result <- write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE, .return_base_compare = TRUE
    ) %>% suppressMessages()

    expect_true(is.list(result))
    expect_s3_class(result$base_df, "data.frame")
    expect_s3_class(result$compare_df, "data.frame")
    expect_false(result$base_from_svn)
    # base should be original data, compare should be modified
    expect_equal(result$base_df$WT[1], nm_s$WT[1])
    expect_equal(result$compare_df$WT[1], nm2$WT[1])
  })
})

test_that(".return_base_compare = FALSE returns invisible NULL", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    result <- write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE, .return_base_compare = FALSE
    ) %>% suppressMessages()

    expect_null(result)
  })
})

# ── Scenario 6: Column changes ───────────────────────────────────────────────

test_that("diff-summary shows column removal in both data and spec sections", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Remove ADAC from both data and spec
    nm2 <- nm_s
    nm2$ADAC <- NULL
    nm_spec2 <- nm_spec_s
    nm_spec2$ADAC <- NULL

    write_derived(
      .data = nm2, .spec = nm_spec2, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("Removed Columns", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Removed", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("ADAC", diff_lines, fixed = TRUE)))
  })
})

test_that("diff-summary shows column addition in both data and spec sections", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")

    # First run without ADAC
    nm_small <- nm_s
    nm_small$ADAC <- NULL
    nm_spec_small <- nm_spec_s
    nm_spec_small$ADAC <- NULL

    write_derived(
      .data = nm_small, .spec = nm_spec_small, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Second run WITH ADAC (adds the column)
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("New Columns", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Added", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("ADAC", diff_lines, fixed = TRUE)))
  })
})

# ── Scenario 7: Row count changes ────────────────────────────────────────────

test_that("diff-summary shows row count increase", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- dplyr::bind_rows(nm_s, nm_s[1:5, ])

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("Rows", diff_lines, fixed = TRUE)))
    expected_row_msg <- paste0(nrow(nm2), " (+5)")
    expect_true(any(grepl(expected_row_msg, diff_lines, fixed = TRUE)))
  })
})

test_that("diff-summary shows row count decrease", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- nm_s[1:(nrow(nm_s) - 5), ]

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("Rows", diff_lines, fixed = TRUE)))
    expected_row_msg <- paste0(nrow(nm2), " (-5)")
    expect_true(any(grepl(expected_row_msg, diff_lines, fixed = TRUE)))
  })
})

# ── Scenario 8: .prev_file parameter ─────────────────────────────────────────

test_that(".prev_file uses specified file for data baseline", {
  withr::with_tempdir({
    # Write original data to a separate baseline file
    baseline_csv <- file.path(getwd(), "baseline.csv")
    write_csv_dots(x = nm_s, file = baseline_csv)

    # Write modified data as the "new" derived dataset
    csv_path <- file.path(getwd(), "pk.csv")
    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 999

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .prev_file = baseline_csv,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("DATA CHANGES", diff_lines, fixed = TRUE)))
    expect_false(any(grepl("No data diffs found", diff_lines, fixed = TRUE)))
  })
})

test_that(".prev_file pointing to non-existent file gives no diffs (like first run)", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")

    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .prev_file = "does-not-exist.csv",
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    expect_false(file.exists(diffs_path))
  })
})

# ── Scenario 9: SVN-based comparison ─────────────────────────────────────────

test_that("SVN comparison includes revision info in diff-summary", {
  skip_if_not(svn_is_available(), "svn not available")

  svn_repo_dir <- local_svn_repo()
  withr::defer(unlink(svn_repo_dir, recursive = TRUE))
  system(paste0("rm -rf ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(paste0("svnadmin create ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)

  svn_wc_dir <- local_svn_repo()
  withr::defer(unlink(svn_wc_dir, recursive = TRUE))

  co_result <- system(
    paste0("svn co file:///", svn_repo_dir, " ", svn_wc_dir, " -q"),
    ignore.stdout = TRUE, ignore.stderr = TRUE
  )
  skip_if(co_result != 0, "svn checkout failed")

  withr::with_dir(svn_wc_dir, {
    csv_path <- file.path(svn_wc_dir, "pk.csv")

    # First run: create all files
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Add everything to SVN and commit
    system("svn add pk.csv pk/ --force -q", ignore.stdout = TRUE, ignore.stderr = TRUE)
    commit_result <- system(
      "svn commit -m 'initial commit' -q",
      ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    skip_if(commit_result != 0, "svn commit failed")

    # Modify data
    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1

    # Second run with SVN comparison
    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = TRUE
    ) %>% suppressMessages()

    diffs_path <- file.path(svn_wc_dir, "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("Repository:", diff_lines, fixed = TRUE)))
    # SVN revision should appear (like "(r1)")
    expect_true(any(grepl("(r", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("DATA CHANGES", diff_lines, fixed = TRUE)))
  })
})

test_that("SVN comparison diffs against committed version, not local", {
  skip_if_not(svn_is_available(), "svn not available")

  svn_repo_dir <- local_svn_repo()
  withr::defer(unlink(svn_repo_dir, recursive = TRUE))
  system(paste0("rm -rf ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(paste0("svnadmin create ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)

  svn_wc_dir <- local_svn_repo()
  withr::defer(unlink(svn_wc_dir, recursive = TRUE))

  co_result <- system(
    paste0("svn co file:///", svn_repo_dir, " ", svn_wc_dir, " -q"),
    ignore.stdout = TRUE, ignore.stderr = TRUE
  )
  skip_if(co_result != 0, "svn checkout failed")

  withr::with_dir(svn_wc_dir, {
    csv_path <- file.path(svn_wc_dir, "pk.csv")

    # First run
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Commit initial version
    system("svn add pk.csv pk/ --force -q", ignore.stdout = TRUE, ignore.stderr = TRUE)
    commit_result <- system(
      "svn commit -m 'initial' -q",
      ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    skip_if(commit_result != 0, "svn commit failed")

    # Modify and write locally (uncommitted intermediate change)
    nm_intermediate <- nm_s
    nm_intermediate$WT[1] <- nm_intermediate$WT[1] + 100
    write_derived(
      .data = nm_intermediate, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Now modify again and compare from SVN
    nm_final <- nm_s
    nm_final$WT[1] <- nm_final$WT[1] + 200

    result <- write_derived(
      .data = nm_final, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = TRUE, .return_base_compare = TRUE
    ) %>% suppressMessages()

    # base_df should be from SVN (original nm_s), not local (nm_intermediate)
    expect_true(result$base_from_svn)
    expect_equal(result$base_df$WT[1], nm_s$WT[1])
  })
})

test_that("SVN comparison with spec changes shows spec diffs against committed spec", {
  skip_if_not(svn_is_available(), "svn not available")

  svn_repo_dir <- local_svn_repo()
  withr::defer(unlink(svn_repo_dir, recursive = TRUE))
  system(paste0("rm -rf ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(paste0("svnadmin create ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)

  svn_wc_dir <- local_svn_repo()
  withr::defer(unlink(svn_wc_dir, recursive = TRUE))

  co_result <- system(
    paste0("svn co file:///", svn_repo_dir, " ", svn_wc_dir, " -q"),
    ignore.stdout = TRUE, ignore.stderr = TRUE
  )
  skip_if(co_result != 0, "svn checkout failed")

  withr::with_dir(svn_wc_dir, {
    csv_path <- file.path(svn_wc_dir, "pk.csv")

    # First run
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Commit everything including spec-list.yml
    system("svn add pk.csv pk/ --force -q", ignore.stdout = TRUE, ignore.stderr = TRUE)
    commit_result <- system(
      "svn commit -m 'initial' -q",
      ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    skip_if(commit_result != 0, "svn commit failed")

    # Modify spec and run with SVN comparison
    nm_spec2 <- nm_spec_s
    nm_spec2$WT$short <- "Modified Weight Label"

    write_derived(
      .data = nm_s, .spec = nm_spec2, .file = csv_path,
      .compare_from_svn = TRUE
    ) %>% suppressMessages()

    diffs_path <- file.path(svn_wc_dir, "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("SPEC CHANGES", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Updated: WT", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("short", diff_lines, fixed = TRUE)))
  })
})

# ── Scenario 10: Diff-summary header format ──────────────────────────────────

test_that("diff-summary header shows local user info", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    diff_lines <- readLines(diffs_path)

    expect_true(any(grepl("DIFF SUMMARY", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Local:", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Repository:", diff_lines, fixed = TRUE)))
    # Local info should contain current user
    local_line <- grep("Local:", diff_lines, value = TRUE)
    expect_true(grepl(Sys.info()[["user"]], local_line))
  })
})

# ── Scenario 11: Per-column value diffs ───────────────────────────────────────

test_that("diff-summary shows per-column diff count when row count is unchanged", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Change values in WT without changing row count
    nm2 <- nm_s
    nm2$WT[1] <- nm2$WT[1] + 1
    nm2$WT[2] <- nm2$WT[2] + 2

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    diff_lines <- readLines(diffs_path)

    # Should show per-column diff counts when row count unchanged
    expect_true(any(grepl("WT", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("diffs", diff_lines, fixed = TRUE)))
  })
})

test_that("diff-summary omits per-column diffs when row count changes", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "pk.csv")
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    # Add rows AND change a value
    nm2 <- dplyr::bind_rows(nm_s, nm_s[1:3, ])
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(
      .data = nm2, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = FALSE
    ) %>% suppressMessages()

    diffs_path <- file.path(getwd(), "pk", "diff-summary.txt")
    diff_lines <- readLines(diffs_path)

    # Per-column diffs (e.g. "2 diffs") should be suppressed when rows change
    expect_true(any(grepl("Rows", diff_lines, fixed = TRUE)))
    expect_false(any(grepl("\\d+ diffs", diff_lines)))
  })
})

# ── Scenario 13: Initial version summary when file not in SVN ────────────────

test_that("write_derived writes initial version summary when file is not in SVN", {
  skip_if_not(svn_is_available(), "svn not available")

  svn_repo_dir <- local_svn_repo()
  withr::defer(unlink(svn_repo_dir, recursive = TRUE))
  system(paste0("rm -rf ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(paste0("svnadmin create ", svn_repo_dir), ignore.stdout = TRUE, ignore.stderr = TRUE)

  svn_wc_dir <- local_svn_repo()
  withr::defer(unlink(svn_wc_dir, recursive = TRUE))

  co_result <- system(
    paste0("svn co file:///", svn_repo_dir, " ", svn_wc_dir, " -q"),
    ignore.stdout = TRUE, ignore.stderr = TRUE
  )
  skip_if(co_result != 0, "svn checkout failed")

  withr::with_dir(svn_wc_dir, {
    csv_path <- file.path(svn_wc_dir, "pk.csv")

    # First run — file not yet committed to SVN
    write_derived(
      .data = nm_s, .spec = nm_spec_s, .file = csv_path,
      .compare_from_svn = TRUE
    ) %>% suppressMessages()

    diffs_path <- file.path(svn_wc_dir, "pk", "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("INITIAL VERSION", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("No prior file in repository", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("DATA SUMMARY", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Rows", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Columns", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Subjects", diff_lines, fixed = TRUE)))
    # Should NOT have Repository line
    expect_false(any(grepl("Repository:", diff_lines, fixed = TRUE)))
  })
})
