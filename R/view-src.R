#' view_src function
#'
#' This function modifies the input dataframe by converting columns with fewer than 20 unique values to factors.
#' It adds labels to the column names and sets up a datatable with a range of options. If the specified
#' subject column is present, it is used to group rows in the table.
#'
#' @param .df A dataframe that you want to process and view.
#' @param .subject_col A character string specifying the subject column, default is "USUBJID". If this column is
#' present in the dataframe, it will be used to group rows in the datatable.
#'
#' @return An instance of DT::datatable class with the processed dataframe.
#' It also includes options for datatable setup such as searchHighlight, scrollX, and pageLength set to TRUE, 5
#' respectively by default. If .subject_col exists in the dataframe, it is used to group rows.
#' The columns with less than 20 unique values are converted to factors. It also modifies column names to include
#' their labels, if any. The labels are extracted from column attributes.
#'
#' @export
view_src <- function(.df, .subject_col = "USUBJID"){

  tableOpts = list(
    pageLength = 5,
    scrollX = TRUE,
    searchHighlight = TRUE
  )

  if (!is.null(.subject_col) & .subject_col %in% names(.df)) {
    colorIndex <- which(names(.df) == .subject_col) - 1
    tableOpts$rowGroup <- list(dataSrc = colorIndex)
  }

  labels <- purrr::map(colnames(.df), ~ attr(.df[[.x]], "label"))

  names_with_labels <-
    paste("<b>", colnames(.df), "</b><br>", "<i>", labels, "</i>", sep = "")

  names(.df) <- names_with_labels

  # Convert columns with less than 20 unique values to factors
  .df <- purrr::map_dfr(.df, ~ {
    if(length(unique(.x)) < 20){
      as.factor(.x)
    } else {
      .x
    }
  })

  # Return the .df table
  DT::datatable(
    .df,
    rownames = FALSE,
    escape = FALSE,
    filter = "top",
    selection = "single",
    class = 'compact cell-border',
    extensions = 'RowGroup',
    options = tableOpts
  )

}
