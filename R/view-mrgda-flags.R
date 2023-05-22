#' View mrgda flags found in spec
#' `r lifecycle::badge("deprecated")`
#'
#' @description
#' Users can manually specify flags in the specification file to describe
#' columns in their data set. `mrgda` also attempts to automatically detect
#' specific columns commonly found in NONMEM data sets.
#'
#'
#' @details
#' Use this function to view a summary of how columns in the data set are
#' currently being described. The output has the following columns:
#'
#' \itemize{
#'  \item{Flag name - flag to be assigned in the spec}
#'  \item{Assigned Columns - data column(s) assigned for each flag}
#'  \item{Description - definition of each flag}
#' }
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .view Logical. Should the output be viewed? (default is TRUE)
#'
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' view_mrgda_flags(.data = nm, .spec = nm_spec)
#'}
#'
#' @keywords internal
view_mrgda_flags <- function(.data, .spec, .view = TRUE) {

  # Check inputs
  stopifnot(".data must be a data.frame" = inherits(.data, "data.frame"))
  stopifnot(".spec must be a yspec object" = inherits(.spec, "yspec"))
  stopifnot(".view must be a logical" = inherits(.view, "logical"))

  gather_return <- gather_flags(.data, .spec, .verbose = TRUE)

  flag_summary <-
    purrr::map_dfr(gather_return$flags, ~ paste(.x, collapse = ", ")) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::left_join(gather_return$recognized_flags) %>%
    dplyr::filter(!name %in% gather_return$missing_flags$name) %>%
    dplyr::select(Flag = name, `Assigned Columns` = value, `mrgda description` = Description) %>%
    dplyr::arrange(Flag) %>%
    suppressMessages()

  if (.view) {
    tibble::view(flag_summary, "Spec flags check")
  } else {
    return(flag_summary)
  }


}
