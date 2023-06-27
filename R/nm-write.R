#' Write an NMTRAN data set and corresponding meta data
#' `r lifecycle::badge("deprecated")`
#'
#' @description
#' `nm_write` is deprecated as of mrgda version 0.7.0. Please use `write_derived` instead.
#'
#' This function will take a nonmem ready data frame in R and write it out to a nonmem ready csv.
#' It also creates a meta data folder, storing the xpt file along with other useful information.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .file csv file name to write out to (including path)
#' @param .prev_file csv file name of previous version (defaults to .file)
#' @param .compare_from_svn logical. Should the data comparison be done on the latest svn version? (If not, local version is used)
#' @param .return_base_compare logical. Should the two current and previous versions of the datasets be returned?
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' nm_write(.data = nm, .spec = nm_spec, .file = "data/derived/pk.csv")
#'}
#' @md
#' @export
nm_write <- function(.data, .spec, .file, .prev_file = NULL, .compare_from_svn = TRUE, .return_base_compare = FALSE) {

  write_derived(
    .data = .data,
    .spec = .spec,
    .file = .file,
    .prev_file = .prev_file,
    .compare_from_svn = .compare_from_svn,
    .return_base_compare = .return_base_compare
  )

}
