#' Mutate ID
#'
#' @description
#' Mutate ID column to a data.frame. The new column will be named `ID`.
#'
#' @param .df data.frame to add ID column to
#' @param .unique_subject_identifier subject identifier
#' @param .start_id numerical value for first ID value
#'
#' @examples
#'
#' test_df <- dplyr::tibble(
#'   USUBJID = c("rand-stud-001", "rand-stud-002", "rand-stud-003"),
#'   WT = c(64, 23, 92),
#'   SEX = c(1, 2, 1))
#'
#' test_df %>%
#' mutate_id(
#'  .unique_subject_identifier = USUBJID,
#'  .start_id = 1
#'  )
#'
#' @export
mutate_id <- function(.df, .unique_subject_identifier, .start_id = 1) {

  assertthat::assert_that(
    !inherits(.df, "grouped_df"),
    msg = "Grouping found in data.frame. This function evaluates a single row at a time. Please ungroup() first."
  )

  message_function("calc_id")

  .df %>%
    dplyr::mutate(
      ID = calc_id(
        .unique_subject_identifier = {{.unique_subject_identifier}},
        .start_id = .start_id
      )
    ) %>%
    dplyr::ungroup()

}
