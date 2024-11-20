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

  # Summarize within subjects to check if columns are constant in each subject
  subject_columns <-
    .df %>%
    dplyr::group_by(!!rlang::sym(.subject_col)) %>%
    # Summarize each column by checking if all values within each subject are identical
    dplyr::summarise(
      dplyr::across(
        tidyselect::everything(),
        ~ dplyr::n_distinct(.x) == 1
      )
    ) %>%
    dplyr::ungroup()

  # Identify columns that are constant within all subjects
  constant_cols <-
    subject_columns %>%
    # For each column (excluding the subject column), check if it is constant across all subjects
    dplyr::summarise(
      dplyr::across(
        -!!rlang::sym(.subject_col),
        ~ all(.x)
      )
    ) %>%
    # Transform the data into a long format with column names and their corresponding boolean values
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "name",
      values_to = "all_constant"
    ) %>%
    # Filter to retain only those columns that are constant within each subject across the entire data frame
    dplyr::filter(all_constant) %>%
    # Extract the names of these columns
    dplyr::pull(name)

  out <-
    if (length(constant_cols) > 0) {
      sort(constant_cols)
    } else {
      "none"
    }

  out
}
