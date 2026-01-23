# Test fixtures
make_src_list <- function(...) {
  domains <- list(...)
  domains$mrgda_labels <- data.frame(DOMAIN = character(), COLUMN_NAME = character(), COLUMN_LABEL = character())
  domains$mrgda_src_meta <- list(md5 = list())
  domains
}

make_df <- function(nrow = 10, ncol = 5, subjects = 3, add_dtc = FALSE, dtc_dates = NULL) {
  df <- as.data.frame(matrix(1, nrow = nrow, ncol = ncol - 1))
  names(df) <- paste0("COL", seq_len(ncol - 1))
  df$USUBJID <- rep(paste0("SUBJ-", seq_len(subjects)), length.out = nrow)

  if (add_dtc) {
    if (is.null(dtc_dates)) {
      dtc_dates <- rep("2024-01-15T10:30:00", nrow)
    }
    df$AESTDTC <- dtc_dates
  }

  df
}


# Input validation tests
test_that("compare_src_lists validates inputs", {
  expect_error(compare_src_lists("not a list", list()), "must be a list")
  expect_error(compare_src_lists(list(), "not a list"), "must be a list")
  expect_error(compare_src_lists(list(), list(), .subject_col = 123), "must be a single character")
  expect_error(compare_src_lists(list(), list(), .subject_col = c("A", "B")), "must be a single character")
})


# Basic comparison tests
test_that("compare_src_lists returns empty tibble for empty lists", {
  result <- compare_src_lists(list(), list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("compare_src_lists detects identical domains", {
  df <- make_df(nrow = 10, ncol = 5, subjects = 3)
  src1 <- make_src_list(dm = df)
  src2 <- make_src_list(dm = df)


  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "dm")
  expect_equal(result$Status, "identical")
  expect_equal(result$Rows, "identical")
  expect_equal(result$Cols, "identical")
})

test_that("compare_src_lists detects added domains", {
  df <- make_df()
  src1 <- make_src_list()
  src2 <- make_src_list(ae = df)

  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "ae")
  expect_equal(result$Status, "added")
  expect_equal(result$Rows, "added")
})

test_that("compare_src_lists detects removed domains", {
  df <- make_df()
  src1 <- make_src_list(ae = df)
  src2 <- make_src_list()

  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "ae")
  expect_equal(result$Status, "removed")
})

test_that("compare_src_lists detects modified domains - row change", {
  df1 <- make_df(nrow = 10)
  df2 <- make_df(nrow = 15)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Rows, "10 -> 15")
})

test_that("compare_src_lists detects modified domains - column change", {
  df1 <- make_df(ncol = 5)
  df2 <- make_df(ncol = 7)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Cols, "5 -> 7")
})

test_that("compare_src_lists detects modified domains - subject change", {
  df1 <- make_df(nrow = 10, subjects = 5)
  df2 <- make_df(nrow = 10, subjects = 8)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Subjects, "5 -> 8")
})


# Multiple domain tests
test_that("compare_src_lists handles multiple domains", {
  df1 <- make_df(nrow = 10)
  df2 <- make_df(nrow = 20)
  src1 <- make_src_list(ae = df1, dm = df1)
  src2 <- make_src_list(ae = df1, dm = df2, lb = df1)

  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 3)
  expect_equal(result$Domain, c("ae", "dm", "lb"))
  expect_equal(result$Status, c("identical", "modified", "added"))
})


# Subject column tests
test_that("compare_src_lists uses custom subject column", {
  df1 <- data.frame(SUBJID = c("A", "A", "B"), X = 1:3)
  df2 <- data.frame(SUBJID = c("A", "B", "C"), X = 1:3)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2, .subject_col = "SUBJID")

  expect_equal(result$Status, "modified")
  expect_equal(result$Subjects, "2 -> 3")
})

test_that("compare_src_lists handles missing subject column", {
  df1 <- data.frame(X = 1:5)
  df2 <- data.frame(X = 1:10)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_true(is.na(result$Subjects))
})


# DTC column tests
test_that("compare_src_lists shows identical for identical domains with DTC", {
  dates <- c("2024-01-01T10:00:00", "2024-06-15T12:30:00", "2024-12-31T23:59:59")
  df <- make_df(nrow = 3, add_dtc = TRUE, dtc_dates = dates)
  src1 <- make_src_list(ae = df)
  src2 <- make_src_list(ae = df)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "identical")
  expect_equal(result$`Date Col`, "identical")
  expect_equal(result$`Date Min`, "identical")
  expect_equal(result$`Date Max`, "identical")
})

test_that("compare_src_lists detects date range changes for modified domains", {
  dates1 <- c("2024-01-01T10:00:00", "2024-06-15T12:30:00")
  dates2 <- c("2024-03-01T10:00:00", "2024-09-15T12:30:00", "2024-10-01T10:00:00")
  df1 <- make_df(nrow = 2, add_dtc = TRUE, dtc_dates = dates1)
  df2 <- make_df(nrow = 3, add_dtc = TRUE, dtc_dates = dates2)
  src1 <- make_src_list(ae = df1)
  src2 <- make_src_list(ae = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$`Date Min`, "2024-01-01 -> 2024-03-01")
  expect_equal(result$`Date Max`, "2024-06-15 -> 2024-10-01")
  expect_equal(result$`Date Col`, "AESTDTC")
})

test_that("compare_src_lists handles domain without DTC columns (identical)", {
  df <- make_df(add_dtc = FALSE)
  src1 <- make_src_list(dm = df)
  src2 <- make_src_list(dm = df)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "identical")
  expect_equal(result$`Date Col`, "identical")
})

test_that("compare_src_lists shows NA for modified domain without DTC columns", {
  df1 <- make_df(nrow = 10, add_dtc = FALSE)
  df2 <- make_df(nrow = 15, add_dtc = FALSE)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_true(is.na(result$`Date Col`))
  expect_true(is.na(result$`Date Min`))
  expect_true(is.na(result$`Date Max`))
})

test_that("compare_src_lists uses first DTC column only", {
  df1 <- data.frame(
    USUBJID = "SUBJ-1",
    X = 1,
    AESTDTC = "2024-01-01T10:00:00",
    AEENDTC = "2024-12-31T23:59:59"
  )
  df2 <- data.frame(
    USUBJID = "SUBJ-1",
    X = 1,
    Y = 2,
    AESTDTC = "2024-01-01T10:00:00",
    AEENDTC = "2024-12-31T23:59:59"
  )
  src1 <- make_src_list(ae = df1)
  src2 <- make_src_list(ae = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$`Date Col`, "AESTDTC")
})


# Row/Subj calculation tests
test_that("compare_src_lists calculates Row/Subj correctly", {
  df1 <- make_df(nrow = 10, subjects = 2)  # 5 rows per subject
  df2 <- make_df(nrow = 20, subjects = 2)  # 10 rows per subject
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$`Row/Subj (%)`, "5 -> 10")
})


# Metadata exclusion tests
test_that("compare_src_lists excludes metadata elements from comparison", {
  df <- make_df()
  src1 <- make_src_list(dm = df)
  src2 <- make_src_list(dm = df)

  result <- compare_src_lists(src1, src2)

  expect_false("mrgda_labels" %in% result$Domain)
  expect_false("mrgda_src_meta" %in% result$Domain)
})


# Helper function tests
test_that("get_dtc_range handles empty data frame", {
  result <- mrgda:::get_dtc_range(data.frame())
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
  expect_true(is.na(result$col))
})

test_that("get_dtc_range handles NULL input", {
  result <- mrgda:::get_dtc_range(NULL)
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
  expect_true(is.na(result$col))
})

test_that("get_dtc_range handles invalid dates", {
  df <- data.frame(AESTDTC = c("invalid-date", "not-a-date-x", ""))
  result <- mrgda:::get_dtc_range(df)
  expect_equal(result$col, "AESTDTC")
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
})

test_that("get_dtc_range handles partial dates", {
  df <- data.frame(AESTDTC = c("2024", "2024-01", "2024-01-15T10:00:00"))
  result <- mrgda:::get_dtc_range(df)
  expect_equal(result$col, "AESTDTC")
  expect_equal(result$min, "2024-01-15")
  expect_equal(result$max, "2024-01-15")
})


test_that("format_date_change handles all cases", {
  expect_true(is.na(mrgda:::format_date_change(NA, NA)))
  expect_equal(mrgda:::format_date_change(NA, "2024-01-01"), "2024-01-01 (new)")
  expect_equal(mrgda:::format_date_change("2024-01-01", NA), "2024-01-01 (removed)")
  expect_equal(mrgda:::format_date_change("2024-01-01", "2024-01-01"), "2024-01-01 (identical)")
  expect_equal(mrgda:::format_date_change("2024-01-01", "2024-12-31"), "2024-01-01 -> 2024-12-31")
})

test_that("format_count_change shows value even when unchanged", {
  expect_true(is.na(mrgda:::format_count_change(NA, NA)))
  expect_equal(mrgda:::format_count_change(NA, 10), "10 (new)")
  expect_equal(mrgda:::format_count_change(10, NA), "10 (removed)")
  expect_equal(mrgda:::format_count_change(10, 10), "10 (identical)")
  expect_equal(mrgda:::format_count_change(10, 20), "10 -> 20")
})

test_that("compare_src_lists shows subject count when unchanged", {
  df1 <- make_df(nrow = 10, subjects = 5)
  df2 <- make_df(nrow = 20, subjects = 5)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Subjects, "5 (identical)")
})
