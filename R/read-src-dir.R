#' Read in every domain in a source data directory.
#'
#' @description
#' This function takes a path to a source data directory (typically SDTM or ADaM folder), reads in every data file, and returns a named list of the data objects.
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'sas7bat'). The default ('detect') will determine file type based on the most occurring file type in .path.
#' @param .read_domains Character vector of domains to read in (e.g. c('dm', 'lb') - default is to load all domains).
#' @param .subject_col Character string of subject identifier column name in source (default is "USUBJID").
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

  .files_of_interest <- list_files_of_type(.path = .path, .file_types = .file_types)

  if (!is.null(.read_domains)) {
    .domains <- tools::file_path_sans_ext(basename(.files_of_interest$files_of_type))
    .domains_keep <- tolower(.domains) %in% tolower(.read_domains)
    .files_read <- .files_of_interest$files_of_type[.domains_keep]
  } else {
    .files_read <- .files_of_interest$files_of_type
  }



  .read_function <-
    if (.files_of_interest$type == "xpt") {
      haven::read_xpt
    } else if (.files_of_interest$type == "sas7bdat") {
      haven::read_sas
    } else if (.files_of_interest$type == "csv") {
      readr::read_csv
    } else {
      stop("'.file_types' must be 'csv', 'sas7bdat', or 'xpt'")
    }


  for (file.i in .files_read) {

    data.i <- try(.read_function(file.i), silent = TRUE)

    if (inherits(data.i, "try-error")) {
      cli::cli_alert_danger(data.i)
    } else {
      cli::cli_alert_success(file.i)
      .out[[tools::file_path_sans_ext(basename(file.i))]] <- data.i
    }

    rm(data.i)

  }


  mrgda_subject <-
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


  if (nrow(mrgda_subject) > 0) {

    mrgda_subject <-
      mrgda_subject %>%
      dplyr::mutate(DOMAIN = tools::file_path_sans_ext(DOMAIN)) %>%
      tidyr::pivot_wider(names_from = DOMAIN, values_from = VALUE)

    mrgda_subject[is.na(mrgda_subject)] <- FALSE

    .out$mrgda_subject <- mrgda_subject

    cli::cli_alert_info(glue::glue("{nrow(.out$mrgda_subject)} unique USUBJID across all domains"))

  } else {

    cli::cli_alert_warning(glue::glue(".subject_col '{.subject_col}' not detected in any domain"))

  }

  return(.out)

}
