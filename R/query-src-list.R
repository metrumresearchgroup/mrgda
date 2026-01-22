#' Query source data domains for a topic
#'
#' @description
#' Search a source list for a string across column names, column labels,
#' column values so you can quickly locate data of interest.
#'
#' @param .src_list A source list created using read_src_dir.
#' @param .string string to search for (not case sensitive by default)
#' @param .ignore_case logical. Should the search not be case sensitive? Defaults to TRUE
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' src_list <- read_src_dir(path, .file_types = "xpt")
#'
#' query_src_list(src_list, .string = "RACE")
#' query_src_list(src_list, .string = "F")
#'
#' @export
query_src_list <- function(.src_list, .string, .ignore_case = TRUE) {
  if (!inherits(.src_list, "list")) {
    stop(
      ".src_list must be a list (preferably the output of `mrgda::read_src_dir()`"
    )
  }
  if (
    missing(.string) ||
      !is.character(.string) ||
      length(.string) != 1 ||
      !nzchar(.string)
  ) {
    stop("'.string' must be a non-empty string")
  }
  if (!is.logical(.ignore_case) || length(.ignore_case) != 1) {
    stop("'.ignore_case' must be a single logical value")
  }

  labels <- .src_list$mrgda_labels
  .src_list$mrgda_labels <- NULL
  .src_list$mrgda_src_meta <- NULL

  labels <- dplyr::mutate(labels, DOMAIN = tolower(.data$DOMAIN))

  col_hits <- labels %>%
    dplyr::filter(grepl(
      .string,
      .data$COLUMN_NAME,
      ignore.case = .ignore_case
    )) %>%
    dplyr::transmute(
      DOMAIN,
      COLUMN = .data$COLUMN_NAME,
      MATCH_TYPE = "column",
      VALUE = NA_character_
    )

  label_hits <- labels %>%
    dplyr::filter(grepl(
      .string,
      .data$COLUMN_LABEL,
      ignore.case = .ignore_case
    )) %>%
    dplyr::transmute(
      DOMAIN,
      COLUMN = .data$COLUMN_NAME,
      MATCH_TYPE = "label",
      VALUE = .data$COLUMN_LABEL
    )

  base_hits <- dplyr::tibble(
    DOMAIN = character(),
    COLUMN = character(),
    MATCH_TYPE = character(),
    VALUE = character()
  )

  value_hits <- dplyr::bind_rows(lapply(names(.src_list), function(domain.i) {
    df.i <- .src_list[[domain.i]]
    if (!is.data.frame(df.i)) {
      return(NULL)
    }

    dplyr::bind_rows(lapply(names(df.i), function(col.i) {
      col_vals <- as.character(df.i[[col.i]])
      match_idx <- grepl(.string, col_vals, ignore.case = .ignore_case)
      if (!any(match_idx, na.rm = TRUE)) {
        return(NULL)
      }

      matched_vals <- unique(col_vals[match_idx & !is.na(col_vals)])
      matched_vals <- matched_vals[matched_vals != ""]
      if (length(matched_vals) == 0) {
        matched_vals <- NA_character_
      }

      dplyr::tibble(
        DOMAIN = tolower(domain.i),
        COLUMN = col.i,
        MATCH_TYPE = "value",
        VALUE = matched_vals
      )
    }))
  }))

  hits <- dplyr::bind_rows(base_hits, col_hits, label_hits, value_hits)

  if (nrow(hits) == 0) {
    cli::cli_alert_danger(paste0("No matches found for ", .string))
    return(invisible(hits))
  }

  hits %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$DOMAIN, .data$MATCH_TYPE, .data$COLUMN)
}
