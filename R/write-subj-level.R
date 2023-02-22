#' Write subject level rds files
#'
#' @description
#' Write out a rds file containing all columns where there is only 1 unique
#' value for each subject (or grouping variable selected)
#'
#' @param .data data.frame containing data
#' @param .subject_identifier column name in data for unique subject identifier
#' @param .path file path to write rds file out to
#'
#' @export
write_subj_level <- function(.data, .subject_identifier, .path) {

  namesDF = dplyr::tibble(name = .subject_identifier)

  for (name.i in names(.data)) {

    .count_row_per_subject <-
      .data %>%
      dplyr::distinct(dplyr::across(c(.subject_identifier, name.i))) %>%
      dplyr::count(dplyr::across(.subject_identifier)) %>%
      dplyr::filter(n > 1)

    if (nrow(.count_row_per_subject) == 0) {
      namesDF <- dplyr::bind_rows(namesDF, dplyr::tibble(name = name.i))
    }
  }

  .data_write <- .data %>% dplyr::select(namesDF$name) %>% dplyr::distinct()

  readr::write_rds(.data_write, file = .path)
}
