#' View mrgda flags found in spec
#'
#' @description
#' Users can manually specify flags in the specification file to describe
#' columns in their data set. `mrgda` also attempts to automatically detect
#' specific columns commonly found in NONMEM data sets.
#'
#' @details
#' Use this function to view a summary of how columns in the data set are
#' currently being described. The output has the following columns:
#'
#' \itemize{
#'  \item{Flag name - flag to be assigned in the spec}
#'  \item{Default Column - data column name `mrgda` uses to automatically detect column}
#'  \item{Detected Columns - data column(s) assigned for each flag}
#'  \item{Description - definition of each flag}
#' }
#'
#' @param .data a data frame
#' @param .spec a yspec object
#'
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' view_spec_flags(.data = nm, .spec = nm_spec)
#'}
#'
#' @export
view_spec_flags <- function(.data, .spec) {

  gather_return <- gather_flags(.data, .spec)

  # make dataframe of all found flags
  found_flags <- dplyr::tibble()
  for (i in names(gather_return$flags)) {
    found_flags <-
      dplyr::bind_rows(
      found_flags,
      dplyr::tibble(
      `Flag name` = i,
      `Detected Columns` = paste(gather_return$flags[[i]], collapse = ", ")
      )
    )
  }

  flags_check <- purrr::map(gather_return$flags, ~is.null(.x))
  names_missing_flags <- names(flags_check[flags_check == TRUE])

  if (length(names_missing_flags) > 0) {
    cli::cli_alert_info("Undefined mrgda flags in spec file:")
    cli::cli_bullets(names_missing_flags)
  }

  flag_summary <- readr::read_csv(system.file("package-data/recognized-flags.csv", package = "mrgda")) %>%
    dplyr::left_join(found_flags) %>%
    dplyr::mutate(
      `Default Column` = dplyr::if_else(is.na(`Default Column`), "No default", `Default Column`),
      `Default Column` = dplyr::if_else(grepl("_", `Default Column`),
                                        gsub(pattern = "_", replacement = " or ", x = `Default Column`),
                                        `Default Column`)
    ) %>%
    dplyr::select(`Flag name`, `Default Column`, `Detected Columns`, Description) %>%
    dplyr::arrange(`Detected Columns`) %>%
    suppressMessages() %>%
    tibble::view("Spec flags check")

}
