library(Rcpp)
library(RcppParallel)

# source the Rcpp function
Rcpp::sourceCpp('~/mrgda/R/search_matches.cpp')

#' Source data query
#'
#' @description
#' Use this to search for a character string across every source data domain including
#' the name, label, and contents of each domain.
#'
#' @param .src_directory file path to the source data directory
#' @param .string string to search for
#' @param .file_types Type of files being read in (e.g. 'sas7bat'). The default ('detect') will determine file type based on the most occurring file type in .path.
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' query_src_dir(.src_directory = path, .string = "RACE")
#'
#' @export
query_src_dir <- function(.src_directory, .string, .file_types = "detect") {

  src_list <- read_src_dir(.path = .src_directory, .file_types = .file_types)
  src_list$mrgda_src_meta <- NULL

  src_list_char <- purrr::map(
    src_list, ~ .x %>% dplyr::mutate_all(as.character)
  )

  matches <- src_list_char[grepl(.string, src_list_char, ignore.case = TRUE)]

  if (length(matches) == 0) {
    stop("No matches found for ", .string)
  }

  # Use the Rcpp function here instead of the nested for-loops
  hits <- findMatchesCppParallel(matches, .string)

  hits %>%
    dplyr::group_by(DOMAIN, COLUMN, VALUE) %>%
    dplyr::summarise(
      MATCHING_ROWS = paste(I, collapse = ", ")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(MATCH = VALUE)
}
