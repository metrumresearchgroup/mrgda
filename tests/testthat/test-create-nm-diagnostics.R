library(testthat)
# Use testthat 3rd edition
testthat::local_edition(3)

library(withr)
library(dplyr)

# Sample data and spec
pk_csv  <- system.file("derived", "pk.csv",  package = "mrgda")
pk_spec <- system.file("derived", "pk.yml", package = "mrgda")
nm       <- readr::read_csv(pk_csv, na = ".")
spec     <- yspec::ys_load(pk_spec)
cols     <- identify_subject_cols(.df = nm, .subject_col = "ID")

# 1. invalid output directory
test_that("errors when output directory does not exist", {
  expect_error(
    create_nm_diagnostics(
      .data = nm[1:5, ],
      .subject_cols = cols,
      .spec = spec,
      .outdir = file.path(tempdir(), "no_such_dir"),
      .filename = "dummy.pdf"
    ),
    "does not exist"
  )
})

# 2. function runs without error when OCC missing
test_that("handles missing OCC column gracefully", {
  df0 <- nm[1:5, ]  # no OCC column in original data
  td <- local_tempdir()
  outfile <- file.path(td, "test.pdf")

  # Should run without error, messages (e.g. PDF written) are expected
  expect_error(
    create_nm_diagnostics(
      .data = df0,
      .subject_cols = cols,
      .spec = spec,
      .subject_col = "ID",
      .outdir = td,
      .filename = "test.pdf"
    ),
    NA
  )

  expect_true(file.exists(outfile))
})

# 3. PDF page count equals number of unique subjects
test_that("creates PDF with one page per subject", {
  skip_if_not_installed("pdftools")
  ids <- unique(nm$ID)[1:3]
  df <- filter(nm, ID %in% ids)
  td <- local_tempdir()
  outfile <- file.path(td, "out.pdf")

  create_nm_diagnostics(
    .data = df,
    .subject_cols = cols,
    .spec = spec,
    .subject_col = "ID",
    .outdir = td,
    .filename = "out.pdf"
  )
  expect_true(file.exists(outfile))

  info <- pdftools::pdf_info(outfile)
  expect_equal(info$pages, length(ids))
})
