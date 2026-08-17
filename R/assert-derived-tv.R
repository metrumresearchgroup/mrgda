#' Assert the structure of time-varying derived data
#'
#' Validate the complete `derived$tv` list before its elements are row-bound.
#' Each element must contain populated `USUBJID` and `DATETIME` columns, and
#' those columns must identify a unique row within that element.
#'
#' The same subject and datetime may occur in different elements. This function
#' checks structure only; it does not check event semantics, allowed values,
#' derivation correctness, or scientific plausibility.
#'
#' @param .tv The complete time-varying list, typically `derived$tv`.
#'
#' @return `.tv`, invisibly.
#'
#' @examples
#' tv <- list(
#'   dosing = tibble::tibble(
#'     USUBJID = c("01", "02"),
#'     DATETIME = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"),
#'     EVID = 1,
#'     AMT = c(100, 100)
#'   ),
#'   pc = tibble::tibble(
#'     USUBJID = c("01", "02"),
#'     DATETIME = as.POSIXct(c("2024-01-01 01:00", "2024-01-02 01:00"), tz = "UTC"),
#'     EVID = 0,
#'     DV = c(10, 20)
#'   )
#' )
#'
#' assert_derived_tv(tv)
#'
#' @export
assert_derived_tv <- function(.tv) {
  # derived$tv must be a nonempty list.
  if (!is.list(.tv) || is.data.frame(.tv) || length(.tv) == 0) {
    cli::cli_abort("{.arg .tv} must be a nonempty named list of data frames")
  }

  # Each list element needs a unique name.
  if (
    is.null(names(.tv)) ||
      anyNA(names(.tv)) ||
      any(!nzchar(names(.tv))) ||
      anyDuplicated(tolower(names(.tv)))
  ) {
    cli::cli_abort("{.arg .tv} must have unique, nonempty names")
  }

  purrr::iwalk(.tv, function(data, name) {
    label <- paste0("derived$tv$", name)

    # Each element must be a data frame.
    if (!is.data.frame(data)) {
      cli::cli_abort("{label} must be a data frame")
    }

    # Grouping must not carry into later sorting or filling.
    if (dplyr::is_grouped_df(data)) {
      cli::cli_abort("{label} must be ungrouped")
    }

    # Each element must contain data.
    if (nrow(data) == 0) {
      cli::cli_abort("{label} must contain at least one row")
    }

    # Column names must be unambiguous.
    if (anyDuplicated(names(data))) {
      cli::cli_abort("{label} must have unique column names")
    }

    # Model-data columns must be atomic.
    if (any(purrr::map_lgl(data, is.list))) {
      cli::cli_abort("{label} must not contain list columns")
    }

    # USUBJID and DATETIME define the row key.
    if (!all(c("USUBJID", "DATETIME") %in% names(data))) {
      cli::cli_abort("{label} must contain USUBJID and DATETIME")
    }

    # Every row needs a subject identifier.
    missing_subject <-
      is.na(data$USUBJID) |
      !nzchar(trimws(as.character(data$USUBJID)))
    if (any(missing_subject)) {
      cli::cli_abort("{label} has missing or blank USUBJID values")
    }

    # Every row needs an event datetime.
    missing_datetime <-
      is.na(data$DATETIME) |
      !nzchar(trimws(as.character(data$DATETIME)))
    if (any(missing_datetime)) {
      cli::cli_abort("{label} has missing or blank DATETIME values")
    }

    # USUBJID and DATETIME must identify one row within this element.
    unique_rows <- dplyr::distinct(data, .data$USUBJID, .data$DATETIME)
    if (nrow(unique_rows) != nrow(data)) {
      cli::cli_abort("{label} has duplicated USUBJID + DATETIME rows")
    }
  })

  # Shared columns must have types that bind_rows() can combine.
  dplyr::bind_rows(.tv)

  invisible(.tv)
}
