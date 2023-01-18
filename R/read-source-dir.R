# For development

# .path <- system.file("example-sdtm", package = "mrgda")



read_source_dir <- function(.path, .file_types = "detect"){

  .out <- list()

  .files <- list.files(.path, full.names = TRUE)

  .extensions <- tools::file_ext(.files)

  if(.file_types == "detect"){

    .file_type_use <- names(which.max(table(.extensions)))

    message("Detected type = .", .file_type_use)

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
      stop("File type must be c('csv', 'sas7bdat', or 'xpt'")
    }


  for (file.i in .files) {

    if (tools::file_ext(file.i) != .file_type_use) {
      next
    }

    message("Reading ", file.i)

    .out[[basename(file.i)]] <- .read_function(file.i)

  }


  return(.out)

}
