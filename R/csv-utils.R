#' Read CSV File
#'
#' This function reads a CSV file formatted for NONMEM (or similar) where missing values
#' are represented by a period (".") and converts these to `NA`. It utilizes the
#'  `data.table::fread` function for fast reading and converts the result to a data frame.
#'
#' @param .file A character string specifying the path to the CSV file to be read.
#'
#' @return A data frame with the contents of the CSV file, with "." values replaced by `NA`.
#'
#' @export
read_nm_csv <- function(.file) {

  df <-
    data.table::fread(
      file = .file,
      sep = ",",
      quote = FALSE,
      na.strings = "."
    ) %>%
    as.data.frame() %>%
    suppressMessages()

  cli::cli_alert_success(paste0("Successfully read: '", .file, "'"))
  cli::cli_alert_info('"." replaced with NA')

  df
}


#' Write csv file
#'
#' @description
#' Writes csv files and formats the data to replace any NA with
#' "." values.
#'
#' @param .data data.frame
#' @param .file file path to write out csv file
#'
#' @noRd
write_nm_csv <- function(.data, .file) {

  data.table::fwrite(
    x = .data,
    file = .file,
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    na = "."
  )

  cli::cli_alert_success(paste0("Successfully wrote: '", .file, "'"))
  cli::cli_alert_info('NA replaced with "."')
}
