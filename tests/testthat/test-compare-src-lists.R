# Test fixtures for src_list_summary and compare_src_lists
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


# src_list_summary tests
test_that("src_list_summary validates inputs", {
  expect_error(src_list_summary("not a list"), "must be a list")
  expect_error(src_list_summary(list(), .subject_col = 123), "must be a single character")
})

test_that("src_list_summary returns empty tibble for empty list", {
  result <- src_list_summary(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("src_list_summary calculates correct stats", {
  df <- make_df(nrow = 12, ncol = 5, subjects = 3)
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "dm")
  expect_equal(result$Rows, 12)
  expect_equal(result$Cols, 5)
  expect_equal(result$Subjects, 3)
  expect_equal(result$`Rows/Subj (ratio)`, 4)
})

test_that("src_list_summary handles multiple domains", {
  df1 <- make_df(nrow = 10, ncol = 3, subjects = 2)
  df2 <- make_df(nrow = 20, ncol = 5, subjects = 4)
  src <- make_src_list(ae = df1, dm = df2)

  result <- src_list_summary(src)

  expect_equal(nrow(result), 2)
  expect_equal(result$Domain, c("ae", "dm"))
})

test_that("src_list_summary extracts date range", {
  dates <- c("2024-01-01T10:00:00", "2024-06-15T12:30:00", "2024-12-31T23:59:59")
  df <- make_df(nrow = 3, add_dtc = TRUE, dtc_dates = dates)
  src <- make_src_list(ae = df)

  result <- src_list_summary(src)

  expect_equal(result$`Date Min`, "2024-01-01")
  expect_equal(result$`Date Max`, "2024-12-31")
  expect_equal(result$`Date Col`, "AESTDTC")
})

test_that("src_list_summary excludes metadata elements", {
  df <- make_df()
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_false("mrgda_labels" %in% result$Domain)
  expect_false("mrgda_src_meta" %in% result$Domain)
})

test_that("src_list_summary returns NA for missing subject column", {
  df <- data.frame(X = 1:10, Y = letters[1:10])
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_true(is.na(result$Subjects))
  expect_true(is.na(result$`Rows/Subj (ratio)`))
})

test_that("src_list_summary returns NA for missing DTC column", {
  df <- make_df(add_dtc = FALSE)
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_true(is.na(result$`Date Min`))
  expect_true(is.na(result$`Date Max`))
  expect_true(is.na(result$`Date Col`))
})

test_that("src_list_summary returns domains in sorted order", {
  df <- make_df()
  src <- make_src_list(zz = df, aa = df, mm = df)

  result <- src_list_summary(src)

  expect_equal(result$Domain, c("aa", "mm", "zz"))
})

test_that("src_list_summary uses custom subject column", {
  df <- data.frame(SUBJID = c("A", "A", "B", "C"), X = 1:4)
  src <- make_src_list(dm = df)

  result <- src_list_summary(src, .subject_col = "SUBJID")

  expect_equal(result$Subjects, 3)
  expect_equal(result$`Rows/Subj (ratio)`, round(4 / 3, 1))
})

test_that("src_list_summary rounds Rows/Subj ratio to 1 decimal", {
  df <- make_df(nrow = 10, subjects = 3)  # 10/3 = 3.333...
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_equal(result$`Rows/Subj (ratio)`, 3.3)
})

test_that("src_list_summary returns correct column types", {
  df <- make_df(nrow = 1000, ncol = 50, subjects = 100, add_dtc = TRUE)
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_type(result$Domain, "character")
  expect_type(result$Rows, "integer")
  expect_type(result$Cols, "integer")
  expect_type(result$Subjects, "integer")
  expect_type(result$`Rows/Subj (ratio)`, "double")
  expect_type(result$`Date Min`, "character")
  expect_type(result$`Date Max`, "character")
  expect_type(result$`Date Col`, "character")
})


# Input validation tests for compare_src_lists
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


# Rows/Subj (ratio) calculation tests
test_that("compare_src_lists calculates Rows/Subj (ratio) correctly", {
  df1 <- make_df(nrow = 10, subjects = 2)  # 5 rows per subject
  df2 <- make_df(nrow = 20, subjects = 2)  # 10 rows per subject
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$`Rows/Subj (ratio)`, "5 -> 10")
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
test_that("extract_dtc_range handles empty data frame", {
  result <- mrgda:::extract_dtc_range(data.frame())
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
  expect_true(is.na(result$col))
})

test_that("extract_dtc_range handles NULL input", {
  result <- mrgda:::extract_dtc_range(NULL)
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
  expect_true(is.na(result$col))
})

test_that("extract_dtc_range handles invalid dates", {
  df <- data.frame(AESTDTC = c("invalid-date", "not-a-date-x", ""))
  result <- mrgda:::extract_dtc_range(df)
  expect_equal(result$col, "AESTDTC")
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
})

test_that("extract_dtc_range handles partial dates", {
  df <- data.frame(AESTDTC = c("2024", "2024-01", "2024-01-15T10:00:00"))
  result <- mrgda:::extract_dtc_range(df)
  expect_equal(result$col, "AESTDTC")
  expect_equal(result$min, "2024-01-15")
  expect_equal(result$max, "2024-01-15")
})


test_that("fmt_diff handles all cases", {
  expect_true(is.na(mrgda:::fmt_diff(NA, NA)))
  expect_equal(mrgda:::fmt_diff(NA, "2024-01-01"), "2024-01-01 (new)")
  expect_equal(mrgda:::fmt_diff("2024-01-01", NA), "2024-01-01 (removed)")
  expect_equal(mrgda:::fmt_diff("2024-01-01", "2024-01-01"), "2024-01-01 (identical)")
  expect_equal(mrgda:::fmt_diff("2024-01-01", "2024-12-31"), "2024-01-01 -> 2024-12-31")
  expect_equal(mrgda:::fmt_diff(10, 10), "10 (identical)")
  expect_equal(mrgda:::fmt_diff(10, 20), "10 -> 20")
})

test_that("fmt_num formats large numbers with commas", {
  expect_equal(mrgda:::fmt_num(1000), "1,000")
  expect_equal(mrgda:::fmt_num(1000000), "1,000,000")
  expect_equal(mrgda:::fmt_num("text"), "text")
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


# Asymmetric DTC column tests
test_that("compare_src_lists handles DTC in list1 but not list2", {
  df1 <- make_df(nrow = 10, add_dtc = TRUE)
  df2 <- make_df(nrow = 15, add_dtc = FALSE)
  src1 <- make_src_list(ae = df1)
  src2 <- make_src_list(ae = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$`Date Min`, "2024-01-15 (removed)")
  expect_equal(result$`Date Max`, "2024-01-15 (removed)")
  expect_equal(result$`Date Col`, "AESTDTC")
})

test_that("compare_src_lists handles DTC in list2 but not list1", {
  df1 <- make_df(nrow = 10, add_dtc = FALSE)
  df2 <- make_df(nrow = 15, add_dtc = TRUE)
  src1 <- make_src_list(ae = df1)
  src2 <- make_src_list(ae = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$`Date Min`, "2024-01-15 (new)")
  expect_equal(result$`Date Max`, "2024-01-15 (new)")
  expect_equal(result$`Date Col`, "AESTDTC")
})


# Asymmetric subject column tests
test_that("compare_src_lists handles subject col in list1 but not list2", {
  df1 <- data.frame(USUBJID = c("A", "B", "C"), X = 1:3)
  df2 <- data.frame(X = 1:5)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Subjects, "3 (removed)")
})

test_that("compare_src_lists handles subject col in list2 but not list1", {
  df1 <- data.frame(X = 1:3)
  df2 <- data.frame(USUBJID = c("A", "B", "C", "D"), X = 1:4)
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Subjects, "4 (new)")
})


# Edge case tests
test_that("compare_src_lists handles empty domain (0 rows)", {
  df1 <- make_df(nrow = 10)
  df2 <- make_df(nrow = 10)[0, ]
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "modified")
  expect_equal(result$Rows, "10 -> 0")
})

test_that("src_list_summary handles empty domain (0 rows)", {
  df <- make_df()[0, ]
  src <- make_src_list(dm = df)

  result <- src_list_summary(src)

  expect_equal(result$Rows, 0)
  expect_equal(result$Subjects, 0)
  expect_true(is.na(result$`Rows/Subj (ratio)`))
})

test_that("compare_src_lists handles decimal Rows/Subj ratio with rounding", {
  df1 <- make_df(nrow = 10, subjects = 3)  # 3.333...
  df2 <- make_df(nrow = 10, subjects = 7)  # 1.428...
  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$`Rows/Subj (ratio)`, "3.3 -> 1.4")
})


# Attribute difference tests
test_that("compare_src_lists marks identical when only attributes differ", {
  df1 <- make_df(nrow = 5, ncol = 3, subjects = 2)
  df2 <- df1
  attr(df2, "custom_attr") <- "some value"
  class(df2) <- c("custom_class", class(df2))

  src1 <- make_src_list(dm = df1)
  src2 <- make_src_list(dm = df2)

  result <- compare_src_lists(src1, src2)

  expect_equal(result$Status, "identical")
})


# Non-data.frame elements in list
test_that("src_list_summary ignores non-data.frame elements", {
  df <- make_df()
  src <- list(
    dm = df,
    some_vector = 1:10,
    some_list = list(a = 1, b = 2),
    mrgda_labels = data.frame(),
    mrgda_src_meta = list()
  )

  result <- src_list_summary(src)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "dm")
})

test_that("compare_src_lists ignores non-data.frame elements", {
  df <- make_df()
  src1 <- list(
    dm = df,
    some_vector = 1:10,
    mrgda_labels = data.frame(),
    mrgda_src_meta = list()
  )
  src2 <- list(
    dm = df,
    different_vector = letters[1:5],
    mrgda_labels = data.frame(),
    mrgda_src_meta = list()
  )

  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 1)
  expect_equal(result$Domain, "dm")
  expect_equal(result$Status, "identical")
})


# All statuses in one comparison
test_that("compare_src_lists handles all statuses in one comparison", {
  df_identical <- make_df(nrow = 10)
  df_modified1 <- make_df(nrow = 10)
  df_modified2 <- make_df(nrow = 20)
  df_added <- make_df(nrow = 5)
  df_removed <- make_df(nrow = 8)


  src1 <- make_src_list(
    ae = df_identical,
    dm = df_modified1,
    lb = df_removed
  )
  src2 <- make_src_list(
    ae = df_identical,
    dm = df_modified2,
    vs = df_added
  )

  result <- compare_src_lists(src1, src2)

  expect_equal(nrow(result), 4)
  expect_equal(result$Domain, c("ae", "dm", "lb", "vs"))
  expect_equal(result$Status, c("identical", "modified", "removed", "added"))
})
