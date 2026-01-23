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
#' - `Rows/Subj (ratio)`: Rows per subject ratio
#' - `Date Min`, `Date Max`: Date range from the first column ending in "DTC"
#' - `Date Col`: Name of the DTC column used
#'
#' @seealso [compare_src_lists()] for comparing two source lists
#'
#' @examples
#' \dontrun{
#' src <- read_src_dir(path, .file_types = "xpt")
#' src_list_summary(src)
#' }
#'
#' @export
src_list_summary <- function(.src_list, .subject_col = "USUBJID") {
  # Validate inputs
  stopifnot(
    "`.src_list` must be a list" = is.list(.src_list),
    "`.subject_col` must be a single character string" = is.character(
      .subject_col
    ) &&
      length(.subject_col) == 1
  )

  # Exclude metadata elements and keep only data frames (domains)
  meta <- c("mrgda_labels", "mrgda_src_meta")
  domains <- .src_list[setdiff(names(.src_list), meta)] %>%
    purrr::keep(is.data.frame)

  # Return empty tibble with correct structure if no domains found
  if (length(domains) == 0) {
    return(tibble::tibble(
      Domain = character(),
      Rows = integer(),
      Cols = integer(),
      Subjects = integer(),
      `Rows/Subj (ratio)` = double(),
      `Date Min` = character(),
      `Date Max` = character(),
      `Date Col` = character()
    ))
  }

  # Iterate over domains in sorted order and compute summary statistics
  purrr::imap_dfr(domains[sort(names(domains))], function(df, domain) {
    # Extract date range from first DTC column
    dtc <- extract_dtc_range(df)

    # Count unique subjects if subject column exists
    nsubj <- if (.subject_col %in% names(df)) {
      dplyr::n_distinct(df[[.subject_col]])
    } else {
      NA_integer_
    }

    # Build summary row for this domain
    tibble::tibble(
      Domain = domain,
      Rows = nrow(df),
      Cols = ncol(df),
      Subjects = nsubj,
      # Calculate rows per subject ratio (useful for detecting data density)
      `Rows/Subj (ratio)` = if (!is.na(nsubj) && nsubj > 0) {
        round(nrow(df) / nsubj, 1)
      } else {
        NA_real_
      },
      `Date Min` = dtc$min,
      `Date Max` = dtc$max,
      `Date Col` = dtc$col
    )
  })
}


#' Extract date range from first DTC column
#' @noRd
extract_dtc_range <- function(df) {
  # Handle empty or NULL data frames
  if (is.null(df) || ncol(df) == 0) {
    return(list(min = NA_character_, max = NA_character_, col = NA_character_))
  }

  # Find first column ending in "DTC" (standard CDISC date-time convention)
  dtc_col <- stringr::str_subset(names(df), "DTC$")[1]
  if (is.na(dtc_col)) {
    return(list(min = NA_character_, max = NA_character_, col = NA_character_))
  }

  # Extract date portion (first 10 chars) from values with sufficient length
  # This handles ISO 8601 datetime formats like "2024-01-15T10:30:00"
  dates <- df[[dtc_col]] %>%
    as.character() %>%
    stringr::str_subset("^.{10,}") %>%
    stringr::str_sub(1, 10)

  # Parse dates safely, filtering out any failures
  parsed <- tryCatch(as.Date(dates), error = function(e) as.Date(NA))
  parsed <- parsed[!is.na(parsed)]

  # Return min/max range and column name used
  list(
    min = if (length(parsed) > 0) as.character(min(parsed)) else NA_character_,
    max = if (length(parsed) > 0) as.character(max(parsed)) else NA_character_,
    col = dtc_col
  )
}
