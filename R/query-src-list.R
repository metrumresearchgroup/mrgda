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
#' src_list <- read_src_dir(path, .file_types = "xpt")
#'
#' # Summarize source directory
#' query_src_list(src_list, .string = "RACE")
#'
#' @export
query_src_list <- function(.src_list, .string) {

  .src_list$mrgda_src_meta <- NULL

  hits <- dplyr::tibble()

  for (df.i in names(.src_list)) {

    df <- .src_list[[df.i]]
    if (!is.data.frame(df)) {
      next
    }

    name_matches <- grepl(.string, names(df), ignore.case = TRUE)

    label_matches <- purrr::map_lgl(
      names(df),
      ~ {
        label.i <- attr(df[[.x]], "label")
        if (is.null(label.i)) {
          return(FALSE)
        }
        grepl(.string, as.character(label.i), ignore.case = TRUE)
      }
    )

    value_matches <- purrr::map_lgl(
      df,
      ~ {
        value_char <- tryCatch(as.character(.x), error = function(e) NA_character_)
        any(grepl(.string, value_char, ignore.case = TRUE), na.rm = TRUE)
      }
    )

    colmatches.i <- names(df)[name_matches | label_matches | value_matches]

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

  if (nrow(hits) == 0) {
    cli::cli_alert_danger(paste0("No matches found for ", .string))
  } else {
    hits %>% dplyr::mutate(MATCHING = .string)
  }

}
