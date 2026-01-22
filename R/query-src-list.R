#' Search source data domains for a topic
#'
#' @description
#' Search a source list for a string across domain names, column names, column labels,
#' and (optionally) column values so you can quickly locate data of interest.
#'
#' @param .src_list A source list created using read_src_dir.
#' @param .string string to search for (not case sensitive by default)
#' @param .include_values logical. Should column values be searched? Defaults to TRUE
#' @param .case_sensitive logical. Should the search be case sensitive? Defaults to FALSE.
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' src_list <- read_src_dir(path, .file_types = "xpt")
#'
#' # Search by column name or label
#' query_src_list(src_list, .string = "RACE", .include_values = FALSE)
#'
#' # Search by actual values (may be slower)
#' query_src_list(src_list, .string = "F")
#'
#' @export
query_src_list <- function(.src_list,
                               .string,
                               .include_values = TRUE,
                               .case_sensitive = FALSE) {

  if (!inherits(.src_list, "list")) {
    stop(".src_list must be a list (preferably the output of `mrgda::read_src_dir()`")
  }
  if (missing(.string) || !is.character(.string) || length(.string) != 1 || !nzchar(.string)) {
    stop("'.string' must be a non-empty string")
  }
  if (!is.logical(.include_values) || length(.include_values) != 1) {
    stop("'.include_values' must be a single logical value")
  }
  if (!is.logical(.case_sensitive) || length(.case_sensitive) != 1) {
    stop("'.case_sensitive' must be a single logical value")
  }

  .src_list$mrgda_src_meta <- NULL
  labels <- .src_list$mrgda_labels
  .src_list$mrgda_labels <- NULL

  labels <- dplyr::mutate(labels, DOMAIN = tolower(.data$DOMAIN))

  ignore_case <- !.case_sensitive

  hits <- dplyr::tibble(
    DOMAIN = character(),
    COLUMN = character(),
    MATCH_TYPE = character(),
    VALUE = character()
  )

  if (nrow(labels) > 0) {
    col_hits <- labels[grepl(.string, labels$COLUMN_NAME, ignore.case = ignore_case), ]
    if (nrow(col_hits) > 0) {
      hits <- dplyr::bind_rows(
        hits,
        dplyr::tibble(
          DOMAIN = col_hits$DOMAIN,
          COLUMN = col_hits$COLUMN_NAME,
          MATCH_TYPE = "column",
          VALUE = NA_character_
        )
      )
    }

    label_hits <- labels[grepl(.string, labels$COLUMN_LABEL, ignore.case = ignore_case), ]
    if (nrow(label_hits) > 0) {
      hits <- dplyr::bind_rows(
        hits,
        dplyr::tibble(
          DOMAIN = label_hits$DOMAIN,
          COLUMN = label_hits$COLUMN_NAME,
          MATCH_TYPE = "label",
          VALUE = label_hits$COLUMN_LABEL
        )
      )
    }
  }

  if (.include_values) {
    for (domain.i in names(.src_list)) {
      df.i <- .src_list[[domain.i]]
      if (!is.data.frame(df.i)) {
        next
      }

      for (col.i in names(df.i)) {
        col_vals <- as.character(df.i[[col.i]])
        match_idx <- grepl(.string, col_vals, ignore.case = ignore_case)

        if (any(match_idx, na.rm = TRUE)) {
          matched_vals <- unique(col_vals[match_idx & !is.na(col_vals)])
          matched_vals <- matched_vals[matched_vals != ""]
          if (length(matched_vals) == 0) {
            matched_vals <- NA_character_
          }

          hits <- dplyr::bind_rows(
            hits,
            dplyr::tibble(
              DOMAIN = tolower(domain.i),
              COLUMN = col.i,
              MATCH_TYPE = "value",
              VALUE = matched_vals
            )
          )
        }
      }
    }
  }

  if (nrow(hits) == 0) {
    cli::cli_alert_danger(paste0("No matches found for ", .string))
    return(invisible(hits))
  }

  hits %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$DOMAIN, .data$MATCH_TYPE, .data$COLUMN)
}
