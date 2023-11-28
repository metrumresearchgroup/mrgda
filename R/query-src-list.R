#' Query a source list.
#'
#' @description
#' Use this to search for a character string across every source data list including
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

  .src_list$mrgda_src_meta <- NULL

  src_list_char <- purrr::map(
    .src_list, ~ .x %>% dplyr::mutate_all(as.character)
  )

  matches <- src_list_char[grepl(.string, src_list_char, ignore.case = TRUE)]

  if (length(matches) == 0) {
    stop("No matches found for ", .string)
  }

  hits <- dplyr::tibble()

  for (df.i in names(matches)) {

    matches.i <- grepl(.string, matches[[df.i]], ignore.case = TRUE)
    colmatches.i <- names(matches[[df.i]])[matches.i]

    if (length(colmatches.i) > 0) {

      cols_collapsed <- paste(colmatches.i, collapse = ",")

      hits <-
        dplyr::bind_rows(
          hits,
          dplyr::tibble(
            DOMAIN = df.i,
            COLUMNS = cols_collapsed
          )
        )

    }
  }

  hits %>% dplyr::mutate(MATCHING = .string)

}
