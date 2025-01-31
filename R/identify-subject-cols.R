#' Identify columns that are constant within subjects
#'
#' This function identifies columns in a data frame that are constant within each subject defined by a specified subject variable.
#'
#' @param .df A data frame to analyze.
#' @param .subject_col A string specifying the name of the subject variable.
#'
#' @return A character vector of column names that are constant within each subject.
#'
#' @keywords internal
identify_subject_cols <- function(.df, .subject_col) {
  if (!.subject_col %in% names(.df)) {
    stop("Subject column '", .subject_col, "' does not exist in the data frame.")
  }

  # Identify columns that are constant within all subjects
  constant_cols <-
    .df %>%
    dplyr::group_by(!!rlang::sym(.subject_col)) %>%
    dplyr::summarise(
      dplyr::across(tidyselect::everything(), ~ dplyr::n_distinct(.x) == 1)
    ) %>%
    dplyr::ungroup() %>%
    # Summarize all columns other than .subject_col
    dplyr::summarise(
      dplyr::across(-!!rlang::sym(.subject_col), ~ all(.x))
    ) %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::filter(value) %>%
    dplyr::pull(name) %>%
    sort()

  if (length(constant_cols) == 0) {
    constant_cols <- "none"
  }

  constant_cols
}
