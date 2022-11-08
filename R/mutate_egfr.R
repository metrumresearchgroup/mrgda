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
#' @export
mutate_egfr <- function(.df, .age, .wt, .serum_creatinine, .sex, .female_value) {

  print("Formula source: https://www.kidney.org/content/ckd-epi-creatinine-equation-2021")

  assertthat::assert_that(
    length(groups(.df)) == 0,
    msg = "Grouping found in data.frame. This function evaluates a single row at a time. Please ungroup() first."
  )

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
    ungroup()

}

