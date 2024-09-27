#' Query a source list.
#'
#' @description
#' Use this to search for a character string across every element in a source data list including
#' the name, label and contents.
#'
#' @param .src_list A source list created using read_src_dir.
#' @param .string string to search for (not case sensitive)
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#'
#' src_list <- read_src_dir(path)
#'
#' # Summarize source directory
#' query_src_list(src_list, .string = "RACE")
#'
#' @export
query_src_list <- function(.src_list, .string) {
  # Remove unnecessary element
  .src_list$mrgda_src_meta <- NULL

  # Find columns containing the string in each data frame
  hits_list <- purrr::map(.src_list, function(df) {
    cols_with_string <- purrr::map_lgl(df, ~ any(grepl(.string, .x, ignore.case = TRUE)))
    names(df)[cols_with_string]
  })

  # Remove entries with no matches
  hits_list <- purrr::keep(hits_list, ~ length(.x) > 0)

  if (length(hits_list) == 0) {
    cli::cli_alert_danger(paste0("No matches found for ", .string))
    return(invisible(NULL))
  }

  # Create the final tibble
  hits <- tibble::tibble(
    DOMAIN = names(hits_list),
    COLUMNS = purrr::map_chr(hits_list, ~ paste(.x, collapse = ",")),
    MATCHING = .string
  )

  return(hits)
}
