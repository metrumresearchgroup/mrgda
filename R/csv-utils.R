#' @keywords internal
mrgda_read_csv <- function(.file) {

  data.table::fread(
    file = .file,
    sep = ",",
    quote = FALSE,
    na = "."
  ) %>%
    as.data.frame() %>%
    suppressMessages()

}

#' @keywords internal
mrgda_write_csv <- function(.data, .file) {

  data.table::fwrite(
    x = .data,
    file = .file,
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    na = "."
  )
}
