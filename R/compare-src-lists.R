#' Compare Two Source Lists
#'
#' Compares two source lists (typically from [read_src_dir()]) and returns
#' a summary of differences in rows, columns, and subjects per domain.
#'
#' @param .src_list1 First source list (the "base" for comparison).
#' @param .src_list2 Second source list (the "compare" target).
#' @param .subject_col Subject ID column name. Defaults to `"USUBJID"`.
#'
#' @return A tibble with one row per domain containing:
#'
#' - `Domain`: Domain name
#' - `Status`: One of "identical", "modified", "added", or "removed"
#' - `Rows`, `Cols`, `Subjects`, `Row/Subj (%)`: Count comparisons
#' - `Date Min`, `Date Max`: Date range from first DTC column
#' - `Date Col`: Name of the DTC column used
#'
#' @examples
#' \dontrun{
#' src1 <- read_src_dir(path1, .file_types = "xpt")
#' src2 <- read_src_dir(path2, .file_types = "xpt")
#' compare_src_lists(src1, src2)
#' }
#'
#' @export
compare_src_lists <- function(.src_list1,
                              .src_list2,
                              .subject_col = "USUBJID") {


  if (!is.list(.src_list1)) {
    stop("`.src_list1` must be a list")
  }
  if (!is.list(.src_list2)) {
    stop("`.src_list2` must be a list")
  }
  if (!is.character(.subject_col) || length(.subject_col) != 1) {
    stop("`.subject_col` must be a single character string")
  }

  meta_elements <- c("mrgda_labels", "mrgda_src_meta")
  domains1 <- setdiff(names(.src_list1), meta_elements)
  domains2 <- setdiff(names(.src_list2), meta_elements)
  all_domains <- sort(unique(c(domains1, domains2)))

  results <- lapply(all_domains, function(domain) {
    df1 <- .src_list1[[domain]]
    df2 <- .src_list2[[domain]]

    stats1 <- get_domain_stats(df1, .subject_col)
    stats2 <- get_domain_stats(df2, .subject_col)

    status <- determine_status(stats1, stats2)

    build_comparison_row(domain, status, stats1, stats2)
  })

  dplyr::bind_rows(results)
}


#' Get domain statistics
#' @param df A data frame or NULL
#' @param subject_col Subject ID column name
#' @return List with nrow, ncol, nsubj, rps, and dtc info
#' @noRd
get_domain_stats <- function(df, subject_col) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(
      exists = FALSE,
      nrow = NA_integer_,
      ncol = NA_integer_,
      nsubj = NA_integer_,
      rps = NA_real_,
      dtc = list(min = NA_character_, max = NA_character_, col = NA_character_)
    ))
  }

  nrow_val <- nrow(df)
  ncol_val <- ncol(df)
  nsubj_val <- if (subject_col %in% names(df)) {
    length(unique(df[[subject_col]]))
  } else {
    NA_integer_
  }

  rps_val <- if (!is.na(nsubj_val) && nsubj_val > 0) {
    round(nrow_val / nsubj_val, 1)
  } else {
    NA_real_
  }

  list(
    exists = TRUE,
    nrow = nrow_val,
    ncol = ncol_val,
    nsubj = nsubj_val,
    rps = rps_val,
    dtc = get_dtc_range(df)
  )
}


#' Determine comparison status
#' @param stats1 Stats from list1
#' @param stats2 Stats from list2
#' @return Status string
#' @noRd
determine_status <- function(stats1, stats2) {
  if (!stats1$exists && stats2$exists) return("added")
  if (stats1$exists && !stats2$exists) return("removed")

  same_dims <- stats1$nrow == stats2$nrow && stats1$ncol == stats2$ncol
  same_subj <- (is.na(stats1$nsubj) && is.na(stats2$nsubj)) ||
    (!is.na(stats1$nsubj) && !is.na(stats2$nsubj) && stats1$nsubj == stats2$nsubj)

  if (same_dims && same_subj) "identical" else "modified"
}


#' Build comparison row for output
#' @param domain Domain name
#' @param status Status string
#' @param stats1 Stats from list1
#' @param stats2 Stats from list2
#' @return Single-row tibble
#' @noRd
build_comparison_row <- function(domain, status, stats1, stats2) {
  if (status %in% c("identical", "added", "removed")) {
    return(dplyr::tibble(
      Domain = domain,
      Status = status,
      Rows = status,
      Cols = status,
      Subjects = status,
      `Row/Subj (%)` = status,
      `Date Min` = status,
      `Date Max` = status,
      `Date Col` = status
    ))
  }

  dtc_col <- if (!is.na(stats2$dtc$col)) stats2$dtc$col else stats1$dtc$col

  dplyr::tibble(
    Domain = domain,
    Status = status,
    Rows = format_change(stats1$nrow, stats2$nrow),
    Cols = format_change(stats1$ncol, stats2$ncol),
    Subjects = format_count_change(stats1$nsubj, stats2$nsubj),
    `Row/Subj (%)` = format_change(stats1$rps, stats2$rps),
    `Date Min` = format_date_change(stats1$dtc$min, stats2$dtc$min),
    `Date Max` = format_date_change(stats1$dtc$max, stats2$dtc$max),
    `Date Col` = dtc_col
  )
}


#' Format value change (show arrow if different, NA if same)
#' @param val1 Value from list1
#' @param val2 Value from list2
#' @return Formatted string or NA
#' @noRd
format_change <- function(val1, val2) {
  if (identical(val1, val2)) return(NA_character_)
  if (is.na(val1) && is.na(val2)) return(NA_character_)
  paste0(val1, " -> ", val2)
}


#' Format count change (always show value, indicate if unchanged)
#' @param val1 Value from list1
#' @param val2 Value from list2
#' @return Formatted string
#' @noRd
format_count_change <- function(val1, val2) {
  if (is.na(val1) && is.na(val2)) return(NA_character_)
  if (is.na(val1)) return(paste0(val2, " (new)"))
  if (is.na(val2)) return(paste0(val1, " (removed)"))
  if (identical(val1, val2)) return(paste0(val2, " (identical)"))
  paste0(val1, " -> ", val2)
}


#' Format date change (always show value with indicator)
#' @param val1 Date from list1
#' @param val2 Date from list2
#' @return Formatted string
#' @noRd
format_date_change <- function(val1, val2) {
  if (is.na(val1) && is.na(val2)) return(NA_character_)
  if (is.na(val1)) return(paste0(val2, " (new)"))
  if (is.na(val2)) return(paste0(val1, " (removed)"))
  if (val1 == val2) return(paste0(val1, " (identical)"))
  paste0(val1, " -> ", val2)
}


#' Extract date range from first DTC column
#' @param df A data frame
#' @return List with min, max, and col
#' @noRd
get_dtc_range <- function(df) {
  empty <- list(min = NA_character_, max = NA_character_, col = NA_character_)

  if (is.null(df) || !is.data.frame(df)) return(empty)

  dtc_cols <- grep("DTC$", names(df), value = TRUE)
  if (length(dtc_cols) == 0) return(empty)

  dtc_col <- dtc_cols[1]
  vals <- as.character(df[[dtc_col]])
  vals <- vals[!is.na(vals) & nzchar(vals) & nchar(vals) >= 10]

  if (length(vals) == 0) {
    return(list(min = NA_character_, max = NA_character_, col = dtc_col))
  }

  dates <- substr(vals, 1, 10)
  parsed <- tryCatch(
    as.Date(dates, format = "%Y-%m-%d"),
    error = function(e) as.Date(NA)
  )
  parsed <- parsed[!is.na(parsed)]

  if (length(parsed) == 0) {
    return(list(min = NA_character_, max = NA_character_, col = dtc_col))
  }

  list(
    min = as.character(min(parsed)),
    max = as.character(max(parsed)),
    col = dtc_col
  )
}
