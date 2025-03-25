#' Generate Subject EDA (Internal)
#'
#' This internal function processes a dataset to extract subject-level data and performs exploratory data analysis
#' by saving example plots in the metadata folder.
#'
#' @param .data A data frame containing the dataset.
#' @param .spec A yspec object used to convert columns to factors.
#' @param .subject_col Subject column name
#' @param .subject_columns A vector of additional subject columns computed externally.
#' @param .meta_data_folder Directory where plots and EDA outputs will be saved. Defaults to the current working directory.
#'
#' @return NULL
#' @keywords internal
generate_subject_eda <- function(.data, .spec, .subject_col, .subject_columns, .meta_data_folder) {

  # Uncomment the following lines for testing purposes:
  # .spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
  # .data <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"),
  #                          na = ".",
  #                          show_col_types = FALSE)
  # .subject_col <- "ID"
  # .subject_columns <- mrgda:::identify_subject_cols(.df = .data, .subject_col = .subject_col)

  # Process subject-level data:
  subject_data <-
    .data %>%
    dplyr::select(dplyr::all_of(c(.subject_col, .subject_columns))) %>%
    dplyr::distinct() %>%
    yspec::ys_factors(.spec) %>%
    dplyr::select(-dplyr::ends_with("_v"))

  # Optionally remove column "C" if it exists
  if ("C" %in% names(subject_data)) {
    subject_data$C <- NULL
  }

  # Extract categorical and continuous data:
  .categorical_data <-
    subject_data %>%
    dplyr::select(dplyr::all_of(.subject_col), dplyr::where(is.factor))

  .continuous_data <-
    subject_data %>%
    dplyr::select(dplyr::all_of(.subject_col), dplyr::where(is.numeric))

  # Continuous EDA
  if (ncol(.continuous_data) > 1) {
    # Put pmplots/tables here - use short name from spec
  }

  # Categorical EDA
  if (ncol(.categorical_data) > 1) {
    # Put pmplots/tables here - use short name from spec
  }

  # save plots/tables in one giant pdf to meta data folder

  invisible(NULL)
}
