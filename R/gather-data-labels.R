#' View source data domains and column labels
#'
#' @description
#' After loading in your source data, use `gather_data_labels()` to view the
#' description of each domain and column.
#'
#' @param .df_list a named list with each source data domain (ideally use the output of `read_src_dir()`)
#'
#' @keywords internal
gather_data_labels <- function(.df_list) {

  labels_summary <- dplyr::tibble()

  for (df.i in names(.df_list)) {

    for (name.i in names(.df_list[[df.i]])) {

      label.i <- attr(.df_list[[df.i]][[name.i]], "label")

      if(is.null(label.i)){
        label.i <- "No label"
      }

      labels_summary <-
        dplyr::bind_rows(
          labels_summary,
          dplyr::tibble(
            DOMAIN = tolower(df.i),
            COLUMN_NAME = name.i,
            COLUMN_LABEL = label.i
          )
        )

    }
  }

  return(labels_summary)
}
