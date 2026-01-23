#' Compare two source lists
#'
#' @description
#' Compare two source lists (typically created by `read_src_dir()`) and return
#' a data frame summarizing differences in rows, columns, and subjects per domain.
#'
#' @param .src_list1 First source list to compare (considered the "base").
#' @param .src_list2 Second source list to compare (considered the "compare").
#' @param .subject_col Character string specifying the subject ID column name.
#'   Defaults to "USUBJID".
#'
#' @return A tibble with one row per domain showing comparison results.
#'
#' @examples
#' \dontrun{
#' src_list1 <- read_src_dir(path1, .file_types = "xpt")
#' src_list2 <- read_src_dir(path2, .file_types = "xpt")
#' compare_src_lists(src_list1, src_list2)
#' }
#'
#' @export
compare_src_lists <- function(.src_list1,
                              .src_list2,
                              .subject_col = "USUBJID") {

  # Input validation
  if (!inherits(.src_list1, "list")) {
    stop("`.src_list1` must be a list (preferably the output of `mrgda::read_src_dir()`)")
  }
  if (!inherits(.src_list2, "list")) {
    stop("`.src_list2` must be a list (preferably the output of `mrgda::read_src_dir()`)")
  }
  if (!is.character(.subject_col) || length(.subject_col) != 1) {
    stop("`.subject_col` must be a single character string")
  }

  # Extract metadata elements
  meta_elements <- c("mrgda_labels", "mrgda_src_meta")

  # Get domain names (excluding metadata)
  domains1 <- setdiff(names(.src_list1), meta_elements)
  domains2 <- setdiff(names(.src_list2), meta_elements)

  # All unique domains
  all_domains <- sort(unique(c(domains1, domains2)))

  # Build comparison data frame
  results <- lapply(all_domains, function(domain.i) {
    df1 <- .src_list1[[domain.i]]
    df2 <- .src_list2[[domain.i]]

    in_list1 <- !is.null(df1) && is.data.frame(df1)
    in_list2 <- !is.null(df2) && is.data.frame(df2)

    # Get counts for list1
    if (in_list1) {
      nrow1 <- nrow(df1)
      ncol1 <- ncol(df1)
      nsubj1 <- if (.subject_col %in% names(df1)) length(unique(df1[[.subject_col]])) else NA_integer_
      rps1 <- if (!is.na(nsubj1) && nsubj1 > 0) round(nrow1 / nsubj1, 1) else NA_real_
      dtc1 <- get_dtc_range(df1)
    } else {
      nrow1 <- NA_integer_
      ncol1 <- NA_integer_
      nsubj1 <- NA_integer_
      rps1 <- NA_real_
      dtc1 <- list(min = NA_character_, max = NA_character_, cols = character(0))
    }

    # Get counts for list2
    if (in_list2) {
      nrow2 <- nrow(df2)
      ncol2 <- ncol(df2)
      nsubj2 <- if (.subject_col %in% names(df2)) length(unique(df2[[.subject_col]])) else NA_integer_
      rps2 <- if (!is.na(nsubj2) && nsubj2 > 0) round(nrow2 / nsubj2, 1) else NA_real_
      dtc2 <- get_dtc_range(df2)
    } else {
      nrow2 <- NA_integer_
      ncol2 <- NA_integer_
      nsubj2 <- NA_integer_
      rps2 <- NA_real_
      dtc2 <- list(min = NA_character_, max = NA_character_, cols = character(0))
    }

    # Determine status
    status <- dplyr::case_when(
      !in_list1 & in_list2 ~ "added",
      in_list1 & !in_list2 ~ "removed",
      nrow1 == nrow2 & ncol1 == ncol2 &
        (is.na(nsubj1) & is.na(nsubj2) | (!is.na(nsubj1) & !is.na(nsubj2) & nsubj1 == nsubj2)) ~ "identical",
      TRUE ~ "modified"
    )

    # For identical/added/removed domains, blank out the details
    if (status %in% c("identical", "added", "removed")) {
      return(dplyr::tibble(
        Domain = domain.i,
        Status = status,
        Rows = NA_character_,
        Cols = NA_character_,
        Subjects = NA_character_,
        `Rec/Subj` = NA_character_,
        `Date Min` = NA_character_,
        `Date Max` = NA_character_,
        `Date Cols` = NA_character_
      ))
    }

    # Format comparison strings - only show if changed
    rows_str <- if (identical(nrow1, nrow2)) NA_character_ else format_comparison(nrow1, nrow2, status)
    cols_str <- if (identical(ncol1, ncol2)) NA_character_ else format_comparison(ncol1, ncol2, status)
    subj_str <- if (identical(nsubj1, nsubj2)) NA_character_ else format_comparison(nsubj1, nsubj2, status)
    rps_str <- if (identical(rps1, rps2)) NA_character_ else format_comparison(rps1, rps2, status)

    # Format date min/max separately - only show if changed
    date_min_str <- if (identical(dtc1$min, dtc2$min)) NA_character_ else format_comparison_char(dtc1$min, dtc2$min, status)
    date_max_str <- if (identical(dtc1$max, dtc2$max)) NA_character_ else format_comparison_char(dtc1$max, dtc2$max, status)

    # Get DTC columns (union of both lists)
    dtc_cols <- unique(c(dtc1$cols, dtc2$cols))
    dtc_cols_str <- if (length(dtc_cols) > 0) paste(dtc_cols, collapse = ", ") else NA_character_

    dplyr::tibble(
      Domain = domain.i,
      Status = status,
      Rows = rows_str,
      Cols = cols_str,
      Subjects = subj_str,
      `Rec/Subj` = rps_str,
      `Date Min` = date_min_str,
      `Date Max` = date_max_str,
      `Date Cols` = dtc_cols_str
    )
  })

  dplyr::bind_rows(results)
}


#' Format numeric comparison as string
#'
#' @param val1 Value from list1
#' @param val2 Value from list2
#' @param status Domain status
#' @return Formatted string
#' @keywords internal
#' @noRd
format_comparison <- function(val1, val2, status) {
  if (status == "added") {
    return(paste0("(+) ", val2))
  }
  if (status == "removed") {
    return(paste0(val1, " (-)"))
  }
  if (is.na(val1) && is.na(val2)) {
    return(NA_character_)
  }
  if (is.na(val1) || is.na(val2)) {
    return(paste0(val1, " -> ", val2))
  }
  if (val1 == val2) {
    return(as.character(val1))
  }
  paste0(val1, " -> ", val2)
}


#' Format character comparison as string
#'
#' @param val1 Value from list1
#' @param val2 Value from list2
#' @param status Domain status
#' @return Formatted string
#' @keywords internal
#' @noRd
format_comparison_char <- function(val1, val2, status) {
  if (status == "added") {
    if (is.na(val2)) return(NA_character_)
    return(paste0("(+) ", val2))
  }
  if (status == "removed") {
    if (is.na(val1)) return(NA_character_)
    return(paste0(val1, " (-)"))
  }
  if (is.na(val1) && is.na(val2)) {
    return(NA_character_)
  }
  if (is.na(val1) || is.na(val2)) {
    v1 <- if (is.na(val1)) "NA" else val1
    v2 <- if (is.na(val2)) "NA" else val2
    return(paste0(v1, " -> ", v2))
  }
  if (val1 == val2) {
    return(val1)
  }
  paste0(val1, " -> ", val2)
}


#' Format date range as string
#'
#' @param min_date Min date
#' @param max_date Max date
#' @return Formatted string like "2020-01-01 to 2020-12-31"
#' @keywords internal
#' @noRd
format_date_range <- function(min_date, max_date) {
  if (is.na(min_date) || is.na(max_date)) {
    return(NA_character_)
  }
  paste0(min_date, " to ", max_date)
}


#' Get date range from DTC columns
#'
#' @param df A data frame
#' @return Named list with min, max dates and column names
#' @keywords internal
#' @noRd
get_dtc_range <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(min = NA_character_, max = NA_character_, cols = character(0)))
  }

  dtc_cols <- grep("DTC$", names(df), value = TRUE)

  if (length(dtc_cols) == 0) {
    return(list(min = NA_character_, max = NA_character_, cols = character(0)))
  }

  # Extract date portion from all DTC columns
  all_dates <- unlist(lapply(dtc_cols, function(col) {
    vals <- as.character(df[[col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    # Only keep values that look like full dates (YYYY-MM-DD)
    vals <- vals[nchar(vals) >= 10]
    substr(vals, 1, 10)
  }))

  if (length(all_dates) == 0) {
    return(list(min = NA_character_, max = NA_character_, cols = dtc_cols))
  }

  # Parse dates, ignoring failures
  parsed <- tryCatch(
    as.Date(all_dates),
    error = function(e) as.Date(NA)
  )
  parsed <- parsed[!is.na(parsed)]

  if (length(parsed) == 0) {
    return(list(min = NA_character_, max = NA_character_, cols = dtc_cols))
  }

  list(
    min = as.character(min(parsed)),
    max = as.character(max(parsed)),
    cols = dtc_cols
  )
}
