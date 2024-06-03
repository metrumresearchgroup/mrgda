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
read_csv_dots <- function(file,
                           sep = ",",
                           quote = "",
                           na.strings = ".",
                           integer64 = "double",
                           data.table = FALSE,
                           ...) {

  data.table::fread(
    file = file,
    sep = sep,
    quote = quote,
    na.strings = na.strings,
    integer64 = integer64,
    data.table = data.table,
    ...
  )
}


#' Write CSV File
#'
#' This function writes a CSV file formatted for NONMEM (or similar) where missing values
#' are converted to a period ("."). It utilizes the `data.table::fwrite` function.
#'
#' @param x Any list of same length vectors (eg. data.frame and data.table)
#' @param file Output file name
#' @param sep Separator between columns, default is ","
#' @param quote Should character fields, factor filds and column names be surrounded by double quotes? Defaults to FALSE
#' @param row.names Boolean. Should row names be written? Defaults to FALSE
#' @param na The string to use for missing values in the data, defaults to "."
#' @param ... arguments passed to [data.table::fwrite()]
#'
#' @export
write_csv_dots <- function(x,
                            file,
                            sep = ",",
                            quote = FALSE,
                            row.names = FALSE,
                            na = ".",
                            ...) {

  data.table::fwrite(
    x = x,
    file = file,
    sep = sep,
    quote = quote,
    row.names = row.names,
    na = na,
    ...
  )
}
