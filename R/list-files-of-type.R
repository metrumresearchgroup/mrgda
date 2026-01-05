#' @keywords internal
list_files_of_type <- function(.path, .file_types) {

  .out <- list()

  .files <- list.files(.path, full.names = TRUE)
  .extensions <- tools::file_ext(.files)

  .file_type_use <- gsub(".", "", .file_types, fixed = TRUE)
  if (!.file_type_use %in% c("csv", "sas7bdat", "xpt")) {
    stop("'.file_types' must be 'csv', 'sas7bdat', or 'xpt'")
  }
  cli::cli_alert_info(paste0("User specified file type = '", .file_type_use, "'"))

  .out$files <- .files
  .out$extensions <- .extensions
  .out$type <- .file_type_use
  .out$files_of_type <- .files[.extensions == .file_type_use]

  return(.out)

}

