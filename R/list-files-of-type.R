#' @keywords internal
list_files_of_type <- function(.path, .file_types) {

  .out <- list()

  .files <- list.files(.path, full.names = TRUE)
  .extensions <- tools::file_ext(.files)

  if (.file_types == "detect") {
    .file_type_use <- names(which.max(table(.extensions)))

    cli::cli_alert_info(paste0("Detected file type = '", .file_type_use, "'"))

    .extensions_not_file_use <- .extensions[!(.extensions %in% .file_type_use)]
    .extensions_not_file_use_important <- .extensions_not_file_use[.extensions_not_file_use %in% c("csv", "sas7bdat", "xpt")]
    if(length(.extensions_not_file_use_important)){


      cli::cli_alert_info(
        paste0("Other files found with common source extension(s)\n",
        paste(.extensions_not_file_use_important, collapse = ", ")
        )
      )

    }

  } else {
    cli::cli_alert_info(paste0("User specified file type = '", .file_type_use, "'"))
    .file_type_use <- gsub(".", "", .file_types, fixed = TRUE)

  }

  .out$files <- .files
  .out$extensions <- .extensions
  .out$type <- .file_type_use
  .out$files_of_type <- .files[.extensions == .file_type_use]

  return(.out)

}


