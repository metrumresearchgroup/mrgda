#' Read csv file
#'
#' @description
#' Reads csv files and formats the data to replace any "." with
#' NA values.
#'
#' @param .file file path to csv file
#'
#' @export
read_nm_csv <- function(.file) {

  if (!file.exists(.file)) {
    stop(.file, " not found")
  }

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
#' @export
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
