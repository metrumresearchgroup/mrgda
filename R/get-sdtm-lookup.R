#' @keywords internal
get_sdtm_lookup <- function() {
  readr::read_csv(system.file("package-data/sdtm-lookup.csv", package = "mrgda")) %>%
    dplyr::mutate(
      DOMAIN = tolower(DOMAIN),
      ALL_COLS = dplyr::if_else(
        !is.na(TIME_COL),
        paste0(UNIQUE_COLS, ",", TIME_COL, ",", .subject_col),
        paste0(UNIQUE_COLS, ",", .subject_col))
    ) %>%
    suppressMessages()
}
