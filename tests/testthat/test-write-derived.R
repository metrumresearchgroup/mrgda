nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

withr::with_tempdir({
  writeLines("Version: 1.0", con = "temp.Rproj")
  svn_dir <- local_svn_repo()
  withr::defer(unlink(svn_dir, recursive = TRUE))

  .temp_csv <- tempfile(fileext = ".csv", tmpdir = svn_dir)
  .temp_script <- tempfile(fileext = ".R", tmpdir = svn_dir)
  writeLines(text = "2 + 2", con = .temp_script)

  .temp_csv <- tempfile(fileext = ".csv")


  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  # Run again to create a diff
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  .csv_in <- readr::read_csv(.temp_csv, na = ".") %>% suppressMessages()
  .xpt_in_name <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),
                         "/",
                         gsub(".csv", ".xpt", basename(.temp_csv), fixed = TRUE))
  .xpt_in <- haven::read_xpt(.xpt_in_name) %>% suppressMessages()
  .xpt_in_labels <- purrr::map(.xpt_in, ~ attr(.x, "label"))
  .dir_folder <- gsub(".csv", "", .temp_csv, fixed = TRUE)
  .spec_saved <- paste0(.dir_folder,"/spec-list.yml")
})

test_that("write_derived write csv: csv is written correctly and matches data", {
  expect_equal(nm, .csv_in)
})

test_that("write_derived write xpt: xpt data includes correct labels", {
  expect_equal(
    yspec::ys_get_short(nm_spec)[order(names(yspec::ys_get_short(nm_spec)))],
    .xpt_in_labels[order(names(.xpt_in_labels))]
  )
})


test_that("write_derived does not write diff-summary.txt when no changes", {
  diffs_path <- paste0(gsub(".csv", "" , .temp_csv, fixed=TRUE), "/diff-summary.txt")
  expect_false(file.exists(diffs_path))
})

test_that("write_derived skips xpt and define when csv and spec are unchanged", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    .meta <- gsub(".csv", "", .csv, fixed = TRUE)
    xpt_time <- file.mtime(file.path(.meta, "pk.xpt"))
    Sys.sleep(1)

    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_equal(file.mtime(file.path(.meta, "pk.xpt")), xpt_time)
  })
})

test_that("write_derived leaves diff-summary.txt untouched on unchanged re-run", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    nm2 <- nm
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(.data = nm2, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    diffs_path <- file.path(gsub(".csv", "", .csv, fixed = TRUE), "diff-summary.txt")
    expect_true(file.exists(diffs_path))
    diffs_mtime <- file.mtime(diffs_path)

    # Re-run with same data — nothing should be touched (no VCS diff)
    write_derived(.data = nm2, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_true(file.exists(diffs_path))
    expect_equal(file.mtime(diffs_path), diffs_mtime)
  })
})

test_that("write_derived regenerates xpt and define when data changes", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    .meta <- gsub(".csv", "", .csv, fixed = TRUE)
    xpt_time <- file.mtime(file.path(.meta, "pk.xpt"))
    Sys.sleep(1)

    nm2 <- nm
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(.data = nm2, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_gt(file.mtime(file.path(.meta, "pk.xpt")), xpt_time)
  })
})

test_that("write_derived also skips define document when unchanged", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    .meta <- gsub(".csv", "", .csv, fixed = TRUE)
    define_files <- list.files(.meta, pattern = "^define", full.names = TRUE)
    skip_if(length(define_files) == 0, "define document was not rendered")
    define_time <- file.mtime(define_files[1])
    Sys.sleep(1)

    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_equal(file.mtime(define_files[1]), define_time)
  })
})

test_that("write_derived preserves existing files in metadata folder across unchanged runs", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    .meta <- gsub(".csv", "", .csv, fixed = TRUE)
    extra_file <- file.path(.meta, "extra-note.txt")
    writeLines("keep me", extra_file)

    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_true(file.exists(extra_file))
    expect_equal(readLines(extra_file), "keep me")
  })
})

test_that("write_derived removes legacy metadata folder with diffs.csv and regenerates", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    .meta <- gsub(".csv", "", .csv, fixed = TRUE)
    writeLines("name,value", file.path(.meta, "diffs.csv"))

    expect_message(
      write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE),
      regexp = "legacy"
    )

    expect_false(file.exists(file.path(.meta, "diffs.csv")))
    expect_true(file.exists(file.path(.meta, "spec-list.yml")))
  })
})

test_that("write_derived writes diff-summary.txt with expected content when data changes", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    nm2 <- nm
    nm2$WT[1] <- nm2$WT[1] + 1

    write_derived(.data = nm2, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    diffs_path <- file.path(gsub(".csv", "", .csv, fixed = TRUE), "diff-summary.txt")
    expect_true(file.exists(diffs_path))

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("Local:", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Repository:", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("DATA CHANGES", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("SPEC CHANGES", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("No spec diffs found", diff_lines, fixed = TRUE)))

    # Body should contain the same row content that would have been written to diffs.csv
    expected_diffs <- execute_data_diffs(
      .base_df = nm,
      .compare_df = nm2,
      .subject_col = "ID",
      .print_output = FALSE
    )$diffs
    expected_rows <- purrr::map2_lgl(
      expected_diffs$name,
      expected_diffs$value,
      ~ any(grepl(.x, diff_lines, fixed = TRUE) & grepl(.y, diff_lines, fixed = TRUE))
    )
    expect_true(all(expected_rows))
  })
})

test_that("write_derived rewrites diff-summary.txt with spec changes when data does not change", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    # Plant a stale diff-summary.txt (simulates leftover from old version or prior change)
    diffs_path <- file.path(gsub(".csv", "", .csv, fixed = TRUE), "diff-summary.txt")
    writeLines(
      c(
        "Local:       by stale-user at 2026-01-01 00:00:00",
        "Repository:  by stale-user",
        "",
        "- stale: stale"
      ),
      diffs_path
    )
    expect_true(file.exists(diffs_path))
    diffs_mtime <- file.mtime(diffs_path)
    Sys.sleep(1)

    # Modify the spec (change a label) but keep data identical
    nm_spec2 <- nm_spec
    nm_spec2$WT$short <- "Modified Weight Label"

    # spec-list.yml changes -> summary should be updated with spec changes
    write_derived(.data = nm, .spec = nm_spec2, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    expect_true(file.exists(diffs_path))
    expect_gt(file.mtime(diffs_path), diffs_mtime)

    diff_lines <- readLines(diffs_path)
    expect_true(any(grepl("SPEC CHANGES", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("Updated: WT", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("short", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("DATA CHANGES", diff_lines, fixed = TRUE)))
    expect_true(any(grepl("No data diffs found", diff_lines, fixed = TRUE)))
  })
})

test_that("write_derived skips diffs when csv is unchanged", {
  withr::with_tempdir({
    .csv <- paste0(getwd(), "/pk.csv")

    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    # Re-run with same data — diffs should be skipped
    write_derived(.data = nm, .spec = nm_spec, .file = .csv, .compare_from_svn = FALSE) %>%
      suppressMessages()

    diffs_path <- file.path(gsub(".csv", "", .csv, fixed = TRUE), "diff-summary.txt")
    expect_false(file.exists(diffs_path))
  })
})


test_that("write_derived works with special characters in file name", {
  .temp_csv <- tempfile(fileext = "-pk.csv")
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  expect_equal(nm, readr::read_csv(.temp_csv, na = ".") %>% suppressMessages())
})

test_that("write_derived gives error if data has comma in any value", {
  df_comma <-
    dplyr::tibble(
      name = c("smith, john", "james", "mark"),
      values = c(1, 2, 3)
      )

  expect_error(write_derived(.data = df_comma, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE))
})

test_that("write_derived works with no ID column in data", {
  nm$ID <- NULL
  nm_spec$ID <- NULL
  .temp_csv2 <- paste0(tempfile(), ".csv")

  expect_error(write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv2, .compare_from_svn = FALSE))
})

test_that("if yspec check fails  write derived does not continue", {
  nm_spec$ADAC <- NULL
  expect_error(write_derived(.data = nm, .spec = nm_spec, .file =  paste0(tempfile(), ".csv"), .compare_from_svn = FALSE))
})

test_that("write_derived outputs spec in meta data folder", {
  expect_true(file.exists(.spec_saved))
})
