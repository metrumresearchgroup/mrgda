#' Determine columns with only one unique per subject.
#'
#' @description
#' Return a data.frame with all columns that have one unique value per subject.
#'
#' @param .data Data.frame
#' @param .subject_col String. column name in data for unique subject identifier.
#'
#' @export
distinct_subject_columns <- function(.data, .subject_col) {

  unique_subject_columns <-
    .data %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(~tidyr::replace_na(.x, "mrgda_NA")) %>%
    dplyr::group_by(!!sym(.subject_col)) %>%
    dplyr::summarise_all(~length(unique(.x))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(-!!sym(.subject_col)) %>%
    dplyr::group_by(name) %>%
    dplyr::filter(all(value == 1)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(name) %>%
    dplyr::pull(name)

  .data %>%
    dplyr::select(dplyr::all_of(c(.subject_col, unique_subject_columns))) %>%
    dplyr::distinct()

}
