#' Summarize a Source List
#'
#' Returns a summary of each domain in a source list, including row counts,
#' column counts, subject counts, and date ranges.
#'
#' @param .src_list A source list (typically from [read_src_dir()]).
#' @param .subject_col Subject ID column name. Defaults to `"USUBJID"`.
#'
#' @return A tibble with one row per domain containing:
#' - `Domain`: Domain name
#' - `Rows`: Number of rows
#' - `Cols`: Number of columns
#' - `Subjects`: Number of unique subjects
#' - `Row/Subj`: Rows per subject ratio
#' - `Date Min`, `Date Max`: Date range from the first column ending in "DTC"
#' - `Date Col`: Name of the DTC column used
#'
#' @examples
#' \dontrun{
#' src <- read_src_dir(path, .file_types = "xpt")
#' src_list_summary(src)
#' }
#'
#' @export
src_list_summary <- function(.src_list, .subject_col = "USUBJID") {
  stopifnot(
    "`.src_list` must be a list" = is.list(.src_list),
    "`.subject_col` must be a single character string" = is.character(.subject_col) && length(.subject_col) == 1

)

  meta <- c("mrgda_labels", "mrgda_src_meta")
  domains <- .src_list[setdiff(names(.src_list), meta)] %>%
    purrr::keep(is.data.frame)

  if (length(domains) == 0) {
    return(tibble::tibble(
      Domain = character(), Rows = integer(), Cols = integer(),
      Subjects = integer(), `Row/Subj` = double(),
      `Date Min` = character(), `Date Max` = character(), `Date Col` = character()
    ))
  }

  purrr::imap_dfr(domains[sort(names(domains))], function(df, domain) {
    dtc <- extract_dtc_range(df)
    nsubj <- if (.subject_col %in% names(df)) dplyr::n_distinct(df[[.subject_col]]) else NA_integer_

    tibble::tibble(
      Domain = domain,
      Rows = nrow(df),
      Cols = ncol(df),
      Subjects = nsubj,
      `Row/Subj` = if (!is.na(nsubj) && nsubj > 0) round(nrow(df) / nsubj, 1) else NA_real_,
      `Date Min` = dtc$min,
      `Date Max` = dtc$max,
      `Date Col` = dtc$col
    )
  })
}


#' Extract date range from first DTC column
#' @noRd
extract_dtc_range <- function(df) {
  empty <- list(min = NA_character_, max = NA_character_, col = NA_character_)

  if (is.null(df) || ncol(df) == 0) return(empty)

  dtc_col <- stringr::str_subset(names(df), "DTC$")[1]
  if (is.na(dtc_col)) return(empty)

  dates <- df[[dtc_col]] %>%
    as.character() %>%
    stringr::str_subset("^.{10,}") %>%
    stringr::str_sub(1, 10)

  if (length(dates) == 0) return(list(min = NA_character_, max = NA_character_, col = dtc_col))

  parsed <- tryCatch(
    as.Date(dates),
    error = function(e) as.Date(NA)
  )
  parsed <- parsed[!is.na(parsed)]

  if (length(parsed) == 0) return(list(min = NA_character_, max = NA_character_, col = dtc_col))

  list(min = as.character(min(parsed)), max = as.character(max(parsed)), col = dtc_col)
}
