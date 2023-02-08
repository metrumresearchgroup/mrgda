#' Read in every domain in a source data directory.
#'
#' @description
#' This function takes a path to a source data directory (typically SDTM or ADaM folder), reads in every data file, and returns a named list of the data objects.
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'sas7bat'). Setting to 'detect' will determine file type based on the most occurring file type in .path.
#' @param .read_domains list domains to read in (default is loading all domains)
#' @param .subject_col character string of subject identifier column name in source (default is "USUBJID")
#'
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' # Read in all source files
#' src_list <- read_src_dir(.path = path, .file_types = "detect")
#'
#' # Read in only "dm" and "lb" domains
#' src_list <- read_src_dir(.path = path, .file_types = "detect", .read_domains = c("dm", "lb"))
#'
#' @md
#' @export
read_src_dir <- function(.path,
                         .file_types = "detect",
                         .read_domains = NULL,
                         .subject_col = "USUBJID") {
  .out <- list()

  .files <- list.files(.path, full.names = TRUE)

  .extensions <- tools::file_ext(.files)

  if (!is.null(.read_domains)) {
    .domains <- tools::file_path_sans_ext(basename(.files))
    .domains_keep <- tolower(.domains) %in% tolower(.read_domains)
    .files_read <- .files[.domains_keep]
  } else {
    .files_read <- .files
  }

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


  for (file.i in .files_read) {
    if (tools::file_ext(file.i) != .file_type_use) {
      next
    }

    data.i <- try(.read_function(file.i))

    if (inherits(data.i, "try-error")) {
      cli::cli_alert_danger(file.i)
    } else {
      cli::cli_alert_success(file.i)
      .out[[tools::file_path_sans_ext(basename(file.i))]] <- data.i
    }

    rm(data.i)

  }


  usubjid <-
    purrr::map_dfr(
      .out,
      ~ {
        if (.subject_col %in% names(.x)) {
          return(
            dplyr::select(.x, .subject_col) %>%
              dplyr::distinct() %>%
              dplyr::mutate(VALUE = TRUE)
          )
        }
      },
      .id = "DOMAIN"
    )


  if (nrow(usubjid) > 0) {

    usubjid <-
      usubjid %>%
      dplyr::mutate(DOMAIN = tools::file_path_sans_ext(DOMAIN)) %>%
      tidyr::pivot_wider(names_from = DOMAIN, values_from = VALUE)

    usubjid[is.na(usubjid)] <- FALSE

    .out$usubjid <- usubjid

    cli::cli_alert_info(glue::glue("{nrow(.out$usubjid)} unique USUBJID across all domains"))

  }

  return(.out)

}
