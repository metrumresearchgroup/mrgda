#' Source data query
#'
#' @description
#' Use this to search across every source data domain and column label for a
#' specific string.
#'
#' @param .src_directory file path to the source data directory
#' @param .string string to search for
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#'
#' # Summarize source directory
#' query_src_dir(.src_directory = path, .string = "RACE")
#'
#' @export
query_src_dir <- function(.src_directory, .string) {

  .src_list <- read_src_dir(.path = .src_directory)
  .src_data_labels <- view_src_data_labels(.src_list)

  .src_data_labels %>%
    dplyr::mutate(
      STRINGALL = paste0(DOMAIN_NAME, DOMAIN_LABEL, COLUMN_NAME, COLUMN_LABEL)
    ) %>%
    dplyr::filter(grepl(.string, STRINGALL)) %>%
    dplyr::select(-STRINGALL) %>%
    tibble::view("SrcDirQuery")


}
