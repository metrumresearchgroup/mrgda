correction_plan <- function(...) {
  tibble::tribble(
    ~domain, ~USUBJID, ~seq_variable, ~seq_value, ~variable,
    ~original_value, ~corrected_value, ~reason,
    ...
  )
}

test_that("apply_src_corrections applies, logs, reports, and prints corrections", {
  report_dir <- withr::local_tempdir()
  report <- file.path(report_dir, "source-corrections.md")
  src <- list(
    pc = tibble::tibble(
      USUBJID = c("X", "X"),
      PCSEQ = c(10L, 11L),
      VISITNUM = c(19, 21),
      PCDTC = c("2024-01-01", "2024-01-02")
    )
  )
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "10", "VISITNUM", "19", "20",
    "Confirmed against collection chronology"
  )

  output <- capture.output(
    ans <- apply_src_corrections(src, corrections, report)
  )

  expect_equal(ans$pc$VISITNUM, c(20, 21))
  expect_equal(nrow(ans$corrections), 1)
  expect_equal(ans$corrections$original_value, "19")
  expect_equal(ans$corrections$corrected_value, "20")
  expect_match(paste(output, collapse = "\n"), "before")
  expect_match(paste(output, collapse = "\n"), "after")
  expect_match(paste(output, collapse = "\n"), "VISITNUM")
  expect_match(paste(output, collapse = "\n"), "source_row")
  expect_match(paste(output, collapse = "\n"), "2024-01-02")

  report_lines <- readLines(report)
  expect_true(any(grepl("Source Corrections", report_lines)))
  expect_true(any(grepl("PCSEQ", report_lines)))
  expect_true(any(grepl("Confirmed against collection chronology", report_lines)))
})

test_that("apply_src_corrections limits context to the corrected subject", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    pc = tibble::tibble(
      USUBJID = c("X", "X", "Y", "Y"),
      PCSEQ = c(1, 2, 1, 2),
      VALUE = c(10, 20, 30, 40),
      PCDTC = c(
        "2024-01-01",
        "2024-01-02",
        "2025-01-01",
        "2025-01-02"
      )
    )
  )
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "2", "VALUE", "20", "21", "Reason"
  )

  output <- capture.output(
    ans <- apply_src_corrections(src, corrections, report, .context = 2)
  )
  printed <- paste(output, collapse = "\n")

  expect_match(printed, "2024-01-01")
  expect_match(printed, "2024-01-02")
  expect_false(grepl("2025-01-01", printed, fixed = TRUE))
  expect_false(grepl("2025-01-02", printed, fixed = TRUE))
})

test_that("apply_src_corrections validates and can suppress context", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(dm = tibble::tibble(USUBJID = "X", AGE = 40))
  corrections <- correction_plan(
    "dm", "X", NA, NA, "AGE", "40", "41", "Reason"
  )

  output <- capture.output(
    ans <- apply_src_corrections(src, corrections, report, .context = 0)
  )
  expect_false(any(grepl("Source context", output, fixed = TRUE)))
  expect_error(
    apply_src_corrections(src, corrections, report, .context = -1),
    "non-negative whole number"
  )
  expect_error(
    apply_src_corrections(src, corrections, report, .context = 1.5),
    "non-negative whole number"
  )
})

test_that("apply_src_corrections can print complete records", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    pc = tibble::tibble(
      USUBJID = "X",
      PCSEQ = 10,
      VISITNUM = 19,
      PCDTC = "2024-01-01"
    )
  )
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "10", "VISITNUM", "19", "20", "Reason"
  )

  output <- capture.output(
    ans <- apply_src_corrections(src, corrections, report, .print = "record")
  )

  expect_match(paste(output, collapse = "\n"), "PCDTC")
  expect_match(paste(output, collapse = "\n"), "2024-01-01")
})

test_that("apply_src_corrections handles no corrections", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(dm = tibble::tibble(USUBJID = "X", AGE = 40))

  ans <- apply_src_corrections(src, NULL, report, .print = "none")

  expect_equal(ans$dm, src$dm)
  expect_named(ans$corrections, src_correction_columns())
  expect_equal(nrow(ans$corrections), 0)
  expect_true(any(grepl("No source corrections were applied", readLines(report))))
})

test_that("apply_src_corrections preserves mrgda source metadata", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    dm = tibble::tibble(USUBJID = "X", AGE = 40),
    mrgda_labels = tibble::tibble(
      DOMAIN = "dm",
      COLUMN_NAME = "AGE",
      COLUMN_LABEL = "Age"
    ),
    mrgda_src_meta = list(type = "xpt", path = "source")
  )

  ans <- apply_src_corrections(src, NULL, report, .print = "none")

  expect_identical(ans$mrgda_labels, src$mrgda_labels)
  expect_identical(ans$mrgda_src_meta, src$mrgda_src_meta)
})

test_that("apply_src_corrections supports domains without sequence variables", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(dm = tibble::tibble(USUBJID = c("X", "Y"), AGE = c(40L, 50L)))
  corrections <- correction_plan(
    "dm", "X", NA, NA, "AGE", "40", "41", "Confirmed age"
  )

  ans <- apply_src_corrections(src, corrections, report, .print = "none")

  expect_equal(ans$dm$AGE, c(41L, 50L))
  expect_true(is.na(ans$corrections$seq_variable))
  expect_true(is.na(ans$corrections$seq_value))
})

test_that("apply_src_corrections accepts another unique within-subject key", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    vs = tibble::tibble(
      USUBJID = c("X", "X"),
      VSTESTCD = c("WEIGHT", "HEIGHT"),
      VSSTRESN = c(70, 180)
    )
  )
  corrections <- correction_plan(
    "vs", "X", "VSTESTCD", "WEIGHT", "VSSTRESN", "70", "71",
    "VSTESTCD is the unique source key for this domain"
  )

  ans <- apply_src_corrections(src, corrections, report, .print = "none")

  expect_equal(ans$vs$VSSTRESN, c(71, 180))
})

test_that("apply_src_corrections converts supported source types", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    dm = tibble::tibble(
      USUBJID = c("A", "B", "C", "D"),
      VALUE = c("old", "2", "FALSE", "2024-01-01")
    ),
    int = tibble::tibble(USUBJID = "A", VALUE = 1L),
    dbl = tibble::tibble(USUBJID = "A", VALUE = 1.5),
    lgl = tibble::tibble(USUBJID = "A", VALUE = FALSE),
    date = tibble::tibble(USUBJID = "A", VALUE = as.Date("2024-01-01"))
  )
  corrections <- correction_plan(
    "dm", "A", NA, NA, "VALUE", "old", "new", "character",
    "int", "A", NA, NA, "VALUE", "1", "2", "integer",
    "dbl", "A", NA, NA, "VALUE", "1.5", "2.5", "double",
    "lgl", "A", NA, NA, "VALUE", "FALSE", "TRUE", "logical",
    "date", "A", NA, NA, "VALUE", "2024-01-01", "2024-01-02", "date"
  )

  ans <- apply_src_corrections(src, corrections, report, .print = "none")

  expect_identical(ans$dm$VALUE[[1]], "new")
  expect_identical(ans$int$VALUE, 2L)
  expect_identical(ans$dbl$VALUE, 2.5)
  expect_identical(ans$lgl$VALUE, TRUE)
  expect_identical(ans$date$VALUE, as.Date("2024-01-02"))
})

test_that("apply_src_corrections verifies unique keys and original values", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  duplicate_src <- list(
    pc = tibble::tibble(
      USUBJID = c("X", "X"),
      PCSEQ = c(10, 10),
      VALUE = c(1, 1)
    )
  )
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "10", "VALUE", "1", "2", "Reason"
  )

  expect_error(
    apply_src_corrections(duplicate_src, corrections, report, .print = "none"),
    "expected exactly one source record; found 2"
  )

  unique_src <- list(
    pc = tibble::tibble(USUBJID = "X", PCSEQ = 10, VALUE = 3)
  )
  expect_error(
    apply_src_corrections(unique_src, corrections, report, .print = "none"),
    "expected original value.*1.*found.*3"
  )
})

test_that("apply_src_corrections preflights every correction before applying", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    pc = tibble::tibble(
      USUBJID = c("X", "X"),
      PCSEQ = c(10, 11),
      VALUE = c(1, 2)
    )
  )
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "10", "VALUE", "1", "3", "Valid",
    "pc", "X", "PCSEQ", "11", "VALUE", "99", "4", "Invalid"
  )

  expect_error(
    apply_src_corrections(src, corrections, report, .print = "none"),
    "expected original value"
  )
  expect_equal(src$pc$VALUE, c(1, 2))
  expect_true(any(grepl("has not completed", readLines(report))))
})

test_that("apply_src_corrections leaves an incomplete report when source reading fails", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")

  expect_error(
    apply_src_corrections(
      .src_list = stop("source read failed"),
      .corrections = NULL,
      .report_file = report,
      .print = "none"
    ),
    "source read failed"
  )

  expect_true(any(grepl("has not completed", readLines(report))))
})

test_that("apply_src_corrections rejects duplicate correction targets", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(pc = tibble::tibble(USUBJID = "X", PCSEQ = 10, VALUE = 1))
  corrections <- correction_plan(
    "pc", "X", "PCSEQ", "10", "VALUE", "1", "2", "First",
    "PC", "X", "pcseq", "10", "value", "1", "3", "Second"
  )

  expect_error(
    apply_src_corrections(src, corrections, report, .print = "none"),
    "duplicate corrections"
  )
})

test_that("apply_src_corrections uses canonical correction order", {
  report <- file.path(withr::local_tempdir(), "source-corrections.md")
  src <- list(
    pc = tibble::tibble(USUBJID = "B", PCSEQ = 2, VALUE = 2),
    dm = tibble::tibble(USUBJID = "A", AGE = 40)
  )
  corrections <- correction_plan(
    "pc", "B", "PCSEQ", "2", "VALUE", "2", "3", "Second key",
    "dm", "A", NA, NA, "AGE", "40", "41", "First key"
  )

  ans <- apply_src_corrections(src, corrections, report, .print = "none")

  expect_equal(ans$corrections$domain, c("dm", "pc"))
})
