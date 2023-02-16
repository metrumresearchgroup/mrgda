#' View summary of source data directory
#'
#' @description
#' This function takes a path to a source data directory (typically SDTM or ADaM folder),
#' reads in the basename of each data file and its size, and returns a dataframe
#' with each domains name, size and description of its contents
#'
#' @param .path Full path to the source data directory.
#' @param .file_types Type of files being read in (e.g. 'sas7bat'). Setting to 'detect' will determine file type based on the most occurring file type in .path.
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#'
#' # Summarize source directory
#' view_src_dir_summary(.path = path)
#'
#' @md
#' @export
view_src_dir_summary <- function(.path, .file_types = "detect") {

  .files_of_interest <- list_files_of_type(.path = .path, .file_types = .file_types)

  .domain_summary <- dplyr::tibble()

  for (file.i in .files_of_interest$files_of_type) {

    domain.i <- tools::file_path_sans_ext(basename(file.i))

    size.i <- file.info(file.i)[["size"]]

    .domain_summary <- dplyr::bind_rows(
      .domain_summary,
      dplyr::tibble(DOMAIN = tolower(domain.i), SIZE_KB = size.i / 1000)
    )

  }

  .domain_ref <-
    system.file("package-data", "sdtm-domains.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    dplyr::mutate(DOMAIN = tolower(DOMAIN)) %>%
    suppressMessages()

  .domain_summary %>%
    dplyr::left_join(.domain_ref) %>%
    suppressMessages() %>%
    tibble::view("Source-Summary")

}
