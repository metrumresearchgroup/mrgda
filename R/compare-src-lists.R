#' Compare Two Source Lists
#'
#' Compares two source lists (typically from [read_src_dir()]) and returns
#' a summary of differences in rows, columns, subjects, and date ranges per domain.
#'
#' @param .src_list1 First source list (the "base" for comparison).
#' @param .src_list2 Second source list (the "compare" target).
#' @param .subject_col Subject ID column name. Defaults to `"USUBJID"`.
#'
#' @return A tibble with one row per domain containing:
#'
#' - `Domain`: Domain name
#' - `Status`: One of `"identical"`, `"modified"`, `"added"`, or `"removed"`
#' - `Rows`: Row count comparison
#' - `Cols`: Column count comparison
#' - `Subjects`: Unique subject count comparison
#' - `Row/Subj (%)`: Rows per subject ratio
#' - `Date Min`, `Date Max`: Date range from the first column ending in "DTC"
#' - `Date Col`: Name of the DTC column used for date range
#'
#' For `"identical"`, `"added"`, or `"removed"` domains, all columns display
#' the status. For `"modified"` domains, values are formatted as:
#' - `"X -> Y"`: Value changed from X to Y
#' - `"X (identical)"`: Value unchanged
#' - `"X (new)"`: Value only in `.src_list2`
#' - `"X (removed)"`: Value only in `.src_list1`
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

  if (!is.list(.src_list1)) stop("`.src_list1` must be a list")
  if (!is.list(.src_list2)) stop("`.src_list2` must be a list")
  if (!is.character(.subject_col) || length(.subject_col) != 1) {
    stop("`.subject_col` must be a single character string")
  }

  meta <- c("mrgda_labels", "mrgda_src_meta")
  all_domains <- sort(unique(c(
    setdiff(names(.src_list1), meta),
    setdiff(names(.src_list2), meta)
  )))

  purrr::map_dfr(all_domains, function(domain) {
    s1 <- get_domain_stats(.src_list1[[domain]], .subject_col)
    s2 <- get_domain_stats(.src_list2[[domain]], .subject_col)

    status <- dplyr::case_when(
      !s1$exists & s2$exists ~ "added",
      s1$exists & !s2$exists ~ "removed",
      s1$nrow == s2$nrow & s1$ncol == s2$ncol &
        (is.na(s1$nsubj) & is.na(s2$nsubj) | identical(s1$nsubj, s2$nsubj)) ~ "identical",
      TRUE ~ "modified"
    )

    if (status != "modified") {
      return(dplyr::tibble(
        Domain = domain, Status = status,
        Rows = status, Cols = status, Subjects = status, `Row/Subj (%)` = status,
        `Date Min` = status, `Date Max` = status, `Date Col` = status
      ))
    }

    dplyr::tibble(
      Domain = domain,
      Status = status,
      Rows = fmt_diff(s1$nrow, s2$nrow),
      Cols = fmt_diff(s1$ncol, s2$ncol),
      Subjects = fmt_diff(s1$nsubj, s2$nsubj),
      `Row/Subj (%)` = fmt_diff(s1$rps, s2$rps),
      `Date Min` = fmt_diff(s1$dtc$min, s2$dtc$min),
      `Date Max` = fmt_diff(s1$dtc$max, s2$dtc$max),
      `Date Col` = dplyr::coalesce(s2$dtc$col, s1$dtc$col)
    )
  })
}


#' Get domain statistics
#' @noRd
get_domain_stats <- function(df, subject_col) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(
      exists = FALSE, nrow = NA_integer_, ncol = NA_integer_,
      nsubj = NA_integer_, rps = NA_real_,
      dtc = list(min = NA_character_, max = NA_character_, col = NA_character_)
    ))
  }

  nsubj <- if (subject_col %in% names(df)) dplyr::n_distinct(df[[subject_col]]) else NA_integer_
  rps <- if (!is.na(nsubj) && nsubj > 0) round(nrow(df) / nsubj, 1) else NA_real_

  list(
    exists = TRUE,
    nrow = nrow(df),
    ncol = ncol(df),
    nsubj = nsubj,
    rps = rps,
    dtc = get_dtc_range(df)
  )
}


#' Format difference between two values
#' @noRd
fmt_diff <- function(v1, v2) {
  dplyr::case_when(
    is.na(v1) & is.na(v2) ~ NA_character_,
    is.na(v1) ~ paste0(v2, " (new)"),
    is.na(v2) ~ paste0(v1, " (removed)"),
    v1 == v2 ~ paste0(v1, " (identical)"),
    TRUE ~ paste0(v1, " -> ", v2)
  )
}


#' Extract date range from first DTC column
#' @noRd
get_dtc_range <- function(df) {
  empty <- list(min = NA_character_, max = NA_character_, col = NA_character_)
  if (is.null(df) || !is.data.frame(df)) return(empty)

 dtc_col <- grep("DTC$", names(df), value = TRUE)[1]
  if (is.na(dtc_col)) return(empty)

  dates <- as.character(df[[dtc_col]])
  dates <- dates[!is.na(dates) & nchar(dates) >= 10]
  if (length(dates) == 0) return(list(min = NA_character_, max = NA_character_, col = dtc_col))

  parsed <- tryCatch(as.Date(substr(dates, 1, 10)), error = function(e) as.Date(NA))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(list(min = NA_character_, max = NA_character_, col = dtc_col))

  list(min = as.character(min(parsed)), max = as.character(max(parsed)), col = dtc_col)
}
