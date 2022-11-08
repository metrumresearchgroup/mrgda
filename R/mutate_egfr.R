#' Calculate EGFR
#'
#' @description
#' Mutate estimated glomerular filtration rate to a data.frame. The new column
#' will be named `EGFR`.
#'
#' @details
#' https://www.kidney.org/content/ckd-epi-creatinine-equation-2021
#'
#' @param .df data.frame to add EGFR column to
#' @param .age age in years
#' @param .wt weight in kilograms
#' @param .serum_creatinine serum creatinine mg/dL
#' @param .sex numeric sex
#' @param .female_value numeric value referencing female subjects
#'
#' @examples
#'
#' test_df <- dplyr::tibble(
#'   AGE = c(45, 82, 73),
#'   WT = c(64, 23, 92),
#'   SC = c(1.02, 1.04, 1.98),
#'   SEX = c(1, 2, 1),
#'   RACE = c(1, 2, 3))
#'
#' test_df %>%
#' mutate_egfr(
#'  .age = AGE,
#'  .wt = WT,
#'  .serum_creatinine = SC,
#'  .sex = SEX,
#'  .female_value = 1
#'  )
#'
#' @export
mutate_egfr <- function(.df, .age, .wt, .serum_creatinine, .sex, .female_value) {

  assertthat::assert_that(
    !inherits(.df, "grouped_df"),
    msg = "Grouping found in data.frame. This function evaluates a single row at a time. Please ungroup() first."
  )

  message_function("calc_egfr")

  .df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      EGFR = calc_egfr(
        .age = {{.age}},
        .wt = {{.wt}},
        .serum_creatinine = {{.serum_creatinine}},
        .sex = {{.sex}},
        .female_value = .female_value
      )
    ) %>%
    dplyr::ungroup()

}

