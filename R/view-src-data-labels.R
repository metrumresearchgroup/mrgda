#' View source data domains and column labels
#'
#' @description
#' After loading in your source data, use `view_src_data_labels()` to view the
#' description of each domain and column.
#'
#' @param .src_list a named list with each source data domain (ideally use the output of `read_src_dir()`)
#'
#' @keywords internal
view_src_data_labels <- function(.src_list) {

  src_summary <- dplyr::tibble()

  for (domain.i in names(.src_list)) {

    .domain_name <- domain.i

    if (is.null(attr(.src_list[[domain.i]], "label"))) {
      .domain_desc <- "Missing"
    } else {
      .domain_desc <- attr(.src_list[[domain.i]], "label")
    }

    for (column.i in names(.src_list[[domain.i]])) {

      .column_name <- column.i

      if (is.null(attr(.src_list[[domain.i]][[column.i]], "label"))) {
        .column_desc <- "Missing"
      } else {
        .column_desc <- attr(.src_list[[domain.i]][[column.i]], "label")
      }

      temp_df <- dplyr::tibble(
        DOMAIN_NAME = .domain_name,
        DOMAIN_LABEL = .domain_desc,
        COLUMN_NAME = .column_name,
        COLUMN_LABEL = .column_desc
      )

      src_summary <- dplyr::bind_rows(src_summary, temp_df)

    }
  }

  return(src_summary)
}
