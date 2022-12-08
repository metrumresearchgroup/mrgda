#' ID formula
#'
#' @param .usubjid subject identifier
#' @param .startid numerical value to start creating ID's
#'
#' @keywords internal
calc_id <- function(.usubjid, .startid = 1) {

  as.numeric(forcats::fct_inorder(.usubjid)) + .startid - 1

}
