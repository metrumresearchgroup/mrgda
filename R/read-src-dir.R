#' Read in every domain in a source data directory.
#'
#' @description
#' This function takes a path to a source data directory (typically SDTM or ADaM folder), reads in every data file, and returns a named list of the data objects.
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'sas7bat'). Setting to 'detect' will determine file type based on the most occurring file type in .path.
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' src_list <- read_src_dir(.path = path)
#' @md
#' @export
read_src_dir <- function(.path, .file_types = "detect") {
  .out <- list()

  .files <- list.files(.path, full.names = TRUE)

  .extensions <- tools::file_ext(.files)

  if (.file_types == "detect") {
    .file_type_use <- names(which.max(table(.extensions)))

    cli::cli_alert_info(paste0("Detected type = '", .file_type_use, "'"))

  } else {
    .file_type_use <- gsub(".", "", .file_types, fixed = TRUE)

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

    data.i <- try(.read_function(file.i))

    if (inherits(data.i, "try-error")) {
      cli::cli_alert_danger(file.i)
    } else {
      cli::cli_alert_success(file.i)
      .out[[basename(file.i)]] <- data.i
    }

    rm(data.i)

  }

  return(.out)

}