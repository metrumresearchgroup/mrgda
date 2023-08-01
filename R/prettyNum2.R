#' Round numeric value with significant digits
#'
#' @param .x numeric value
#' @param .digits number of digits to round numeric value to
#'
#' @return numeric value
#'
#' @keywords internal
prettyNum2 <- function(.x, .digits = 3) {
  ifelse(
    .x >= 1,
    round(.x, digits = .digits),
    signif(.x, digits = .digits)
  )
}
