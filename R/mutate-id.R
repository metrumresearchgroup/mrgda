#' Mutate ID
#'
#' @description
#' Mutate ID column to a data.frame. The new column will be named `ID`.
#'
#' @param .df data.frame to add ID column to
#' @param .usubjid subject identifier
#' @param .startid numerical value for first ID value
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
#'  .usubjid = USUBJID,
#'  .startid = 1
#'  )
#'
#' @export
mutate_id <- function(.df, .usubjid, .startid = 1) {

  assertthat::assert_that(
    !inherits(.df, "grouped_df"),
    msg = "Grouping found in data.frame. This function evaluates a single row at a time. Please ungroup() first."
  )

  .df %>%
    dplyr::mutate(
      ID = calc_id(
        .usubjid = {{.usubjid}},
        .startid = .startid
      )
    ) %>%
    dplyr::ungroup()

}
