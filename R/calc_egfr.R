#' EGFR formula
#'
#' @param .age age in years
#' @param .wt weight in kilograms
#' @param .serum_creatinine serum creatinine mg/dL
#' @param .sex numeric value representing male & female
#' @param .female_value numeric representing female subjects
#'
#' @keywords internal
calc_egfr <- function(.age, .wt, .serum_creatinine, .sex, .female_value) {

  .alpha <- dplyr::if_else(.sex == .female_value, -0.241, -0.302)
  .k <- dplyr::if_else(.sex == .female_value, 0.7, 0.9)

  142 *
    (min(.serum_creatinine / .k, 1)^.alpha) *
    (max(.serum_creatinine / .k, 1)^-1.200) *
    (0.9938^.age) *
    (dplyr::if_else(.sex == .female_value, 1.012, 1))
}
