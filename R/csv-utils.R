#' Read CSV File
#'
#' This function reads a CSV file formatted for NONMEM (or similar) where missing values
#' are represented by a period (".") and converts these to `NA`. It utilizes the
#'  `data.table::fread` function for fast reading and converts the result to a data frame.
#'
#' @param file A character string specifying the path to the CSV file to be read.
#' @param sep Separator between columns, defaults to ","
#' @param quote Set to handle fields starting with a double quote, defaults to FALSE
#' @param na.strings A character vector of strings to be interpreted as NA values, defaults to "."
#' @param integer64 Reads columns detected as containing integers larger than 2^31, default to "double"
#' @param data.table Boolean, set to TRUE to return data.table, set to FALSE to return data.frame
#' @param ... arguments passed to [data.table::fread()]
#'
#' @return A data frame with the contents of the CSV file, with "." values replaced by `NA`.
#'
#' @export
read_csv_mrgda <- function(file,
                           sep = ",",
                           quote = FALSE,
                           na.strings = ".",
                           integer64 = "double",
                           data.table = FALSE,
                           ...) {

  data.table::fread(
    file,
    sep = ",",
    quote = FALSE,
    na.strings = ".",
    integer64 = "double",
    data.table = FALSE,
    ...
  )
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
}
