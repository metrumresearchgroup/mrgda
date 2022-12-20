#' ID formula
#'
#' @param .unique_subject_identifier subject identifier
#' @param .start_id numerical value to start creating ID's
#'
#' @keywords internal
calc_id <- function(.unique_subject_identifier, .start_id = 1) {

  as.numeric(forcats::fct_inorder(.unique_subject_identifier)) + .start_id - 1

}
