#' Assert the structure of subject-level derived data
#'
#' Validate the complete `derived$sl` list before its elements are joined.
#' Each element must contain exactly `USUBJID` and one model variable, with one
#' populated row per subject. The element name must match the model variable
#' without regard to case.
#'
#' This function checks structure only. It does not check model-variable
#' missingness, subject coverage across elements, allowed values, derivation
#' correctness, or scientific plausibility.
#'
#' @param .sl The complete subject-level list, typically `derived$sl`.
#'
#' @return `.sl`, invisibly.
#'
#' @examples
#' sl <- list(
#'   sex = tibble::tibble(USUBJID = c("01", "02"), SEX = c(1, 2)),
#'   blwt = tibble::tibble(USUBJID = c("01", "02"), BLWT = c(70, 80))
#' )
#'
#' assert_derived_sl(sl)
#'
#' @export
assert_derived_sl <- function(.sl) {
  # derived$sl must be a nonempty list.
  if (!is.list(.sl) || is.data.frame(.sl) || length(.sl) == 0) {
    cli::cli_abort("{.arg .sl} must be a nonempty named list of data frames")
  }

  # Each list element needs a unique name.
  if (
    is.null(names(.sl)) ||
      anyNA(names(.sl)) ||
      any(!nzchar(names(.sl))) ||
      anyDuplicated(tolower(names(.sl)))
  ) {
    cli::cli_abort("{.arg .sl} must have unique, nonempty names")
  }

  purrr::iwalk(.sl, function(data, name) {
    label <- paste0("derived$sl$", name)

    # Each element must be a data frame.
    if (!is.data.frame(data)) {
      cli::cli_abort("{label} must be a data frame")
    }

    # Grouping must not carry into later joins.
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

    # USUBJID is the subject-level join key.
    if (!("USUBJID" %in% names(data))) {
      cli::cli_abort("{label} must contain USUBJID")
    }

    # Each element stores one model variable.
    model_variable <- setdiff(names(data), "USUBJID")
    if (length(model_variable) != 1) {
      cli::cli_abort("{label} must contain USUBJID and one model variable")
    }

    # The list name must identify its model variable.
    if (tolower(name) != tolower(model_variable)) {
      cli::cli_abort("{label} must be named for model variable {model_variable}")
    }

    # Every row needs a subject identifier.
    missing_subject <-
      is.na(data$USUBJID) |
      !nzchar(trimws(as.character(data$USUBJID)))
    if (any(missing_subject)) {
      cli::cli_abort("{label} has missing or blank USUBJID values")
    }

    # Each subject can appear only once in an element.
    if (anyDuplicated(data$USUBJID)) {
      cli::cli_abort("{label} has duplicated USUBJID values")
    }
  })

  invisible(.sl)
}
