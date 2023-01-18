#' Read in a source data directory (e.g. a folder filled with SDTM domains).
#'
#' @description
#' This function will take a nonmem ready data frame in R and write it out to a nonmem ready csv.
#' It also creates a meta data folder, storing the xpt file along with other useful information.
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'xpt').
#' Setting to 'detect' will determine file type based on the most occurring file type in .path.
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' src_list <- read_src_dir(.path = path)
#' @md
#' @export
read_src_dir <- function(.path, .file_types = "detect"){

  .out <- list()

  .files <- list.files(.path, full.names = TRUE)

  .extensions <- tools::file_ext(.files)

  if (.file_types == "detect") {

    .file_type_use <- names(which.max(table(.extensions)))

    cli::cli_alert_info(paste0("Detected type = '", .file_type_use, "'"))

  } else {

    .file_type_use <- .file_types

  }


  .read_function <-
    if (.file_type_use == "xpt") {
      haven::read_xpt
    } else if (.file_type_use == "sas7bdat") {
      haven::read_sas
    } else if (.file_type_use == "csv") {
      readr::read_csv
    } else {
      stop("'.file_types' must be 'csv', 'sas7bdat', or 'xpt'")
    }


  for (file.i in .files) {

    if (tools::file_ext(file.i) != .file_type_use) {
      next
    }

    .out[[basename(file.i)]] <- .read_function(file.i)

    cli::cli_alert_success(file.i)

  }


  return(.out)

}
