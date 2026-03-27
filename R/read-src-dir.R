#' Read in every domain in a source data directory.
#'
#' @description
#' This function takes a path to a source data directory (typically SDTM or ADaM folder), reads in every data file, and returns a named list of the data objects.
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'sas7bat').
#' @param .read_domains Character vector of domains to read in (e.g. c('dm', 'lb') - default is to load all domains).
#'
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' # Read in all source files
#' src_list <- read_src_dir(.path = path, .file_types = "xpt")
#'
#' # Read in only "dm" and "lb" domains
#' src_list <- read_src_dir(.path = path, .file_types = "xpt", .read_domains = c("dm", "lb"))
#'
#' @md
#' @export
read_src_dir <- function(.path,
                         .file_types,
                         .read_domains = NULL) {

  if (missing(.file_types) || is.null(.file_types)) {
    stop("'.file_types' is required. Must be 'csv', 'sas7bdat', or 'xpt'")
  }
  .file_types <- tolower(.file_types)
  valid_file_types <- c("csv", "sas7bdat", "xpt")
  if (!(.file_types %in% valid_file_types)) {
    stop("'.file_types' must be 'csv', 'sas7bdat', or 'xpt'")
  }

  .out <- list()
  md5_hashes <- list()

  .files_of_interest <- list_files_of_type(.path = .path, .file_types = .file_types)

  if (length(.files_of_interest$files_of_type) == 0) {
    stop(paste0("No files of type '", .file_types, "' found in the specified path."))
  }

  if (!is.null(.read_domains)) {
    .domains <- tools::file_path_sans_ext(basename(.files_of_interest$files_of_type))

    missing_domains <- setdiff(tolower(.read_domains), tolower(.domains))
    if (length(missing_domains) > 0) {
      cli::cli_abort(paste0("The following requested domains could not be found: ", paste(missing_domains, collapse = ", ")))
    }

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

  .file_sizes <- dplyr::tibble()

  for (file.i in .files_read) {

    .file_sizes <-
      dplyr::bind_rows(
        .file_sizes,
        dplyr::tibble(
          FILE = file.i,
          SIZE = file.info(file.i)[["size"]]
        )
      )

  }

  for (file.i in .files_read) {

    .file_size_kb <- .file_sizes$SIZE[.file_sizes$FILE == file.i] / 1000

    cli::cli_progress_message(crayon::yellow(paste0("Reading in ", basename(file.i), " (", .file_size_kb, " KB)")))
    data.i <- try(.read_function(file.i), silent = TRUE)

    if (inherits(data.i, "try-error")) {
      cli::cli_abort(paste0("Failed to load file: ", file.i, "\nOriginal error: ", attr(data.i, "condition")$message))
    }

    cli::cli_alert_success(file.i)
    domain_name.i <- tools::file_path_sans_ext(basename(file.i))
    .out[[domain_name.i]] <- data.i
    md5_hashes[[domain_name.i]] <- unname(tools::md5sum(file.i))

    rm(data.i)

  }


  # mrgda labels ------------------------------------------------------------
  .out$mrgda_labels <-
    gather_data_labels(.out)

  # metadata
  .out$mrgda_src_meta$md5  <- md5_hashes
  .out$mrgda_src_meta$type <- .files_of_interest$type
  .out$mrgda_src_meta$path <- .path

  return(.out)

}
