#' create_datatable function
#'
#' This function modifies the input dataframe by converting columns with fewer than 20 unique values to factors.
#' It adds labels to the column names and sets up a datatable with a range of options. If the specified
#' subject column is present, it is used to group rows in the table.
#'
#' @param .df A dataframe that you want to process and view.
#' @param .subject_col A character string specifying the subject column. If this column is
#' present in the dataframe, it will be used to group rows in the datatable.
#' @param .view Defaults to `"viewer"`. If `"window"` is specified, the table will show in a separate browser window.
#' @param .full_width Logical (`TRUE`/`FALSE`). If `TRUE`, the table will be stretched horizontally to fill the
#' available space.
#' @param .wrap_col_len Numeric value for wrapping long text. Text with more characters than this value will be
#' wrapped to the next line.
#'
#' @return An instance of DT::datatable class with the processed dataframe.
#' It also includes options for datatable setup such as searchHighlight, scrollX, and pageLength set to TRUE, 5
#' respectively by default. If .subject_col exists in the dataframe, it is used to group rows.
#' The columns with less than 20 unique values are converted to factors. It also modifies column names to include
#' their labels, if any. The labels are extracted from column attributes.
#'
#' @importFrom dplyr across
#'
#' @keywords internal
view_src <- function(
    .df,
    .subject_col = NULL,
    .view = c("viewer", "window"),
    .full_width = FALSE,
    .wrap_col_len = 50
){

  .view <- match.arg(.view)

  # Determine widths
  if(isTRUE(.full_width)){
    col_width <- paste0(100/ncol(.df), "%")
  }else{
    col_width <- "30px"
  }

  # Wrap columns over a specified length
  wrap_names <- names(which(purrr::map(.df, ~max(nchar(.x))) > .wrap_col_len))
  .df <- .df %>% dplyr::mutate(across(wrap_names, ~ gsub("\n", "<br>", stringr::str_wrap(.x, .wrap_col_len))))


  tableOpts = list(
    # dom = "B<lf<\"datatables-scroll\"t>ipr>",
    dom = "B<lf<t>ipr>",
    pageLength = 100,
    lengthMenu = c(25,100,500,1000),
    initComplete = htmlwidgets::JS("
    function(settings, json) {
      $(this.api().table()).css({
        'border': '0px'
      });
    }"),
    scrolly = "15vh",
    # paging = FALSE,
    scrollX = TRUE,
    scrollY = 300,
    scrollCollapse=TRUE,
    colReorder = TRUE,
    searchHighlight = TRUE,
    autoWidth = TRUE,
    select = list(style = 'os', items = 'row'),
    buttons = list('colvis'),
    columnDefs = list(list(className = 'dt-center', targets = "_all"),
                      list(width = col_width, targets = "_all"),
                      list(type = 'natural', targets = "_all")
    )
  )

  if (!is.null(.subject_col) && .subject_col %in% names(.df)) {
    .df <- .df %>% dplyr::relocate(!!sym(.subject_col))
    # colorIndex <- which(names(.df) == .subject_col)
    # tableOpts$rowGroup <- list(dataSrc = colorIndex - 1)
    # if(colorIndex != 0) colorIndex <- 0:colorIndex
    tableOpts$fixedColumns <- list(leftColumns = 1)
  }

  # Convert columns with less than 20 unique values to factors
  .df <- purrr::map_dfr(.df, ~ {
    if(length(unique(.x)) < 20){
      as.factor(.x)
    } else {
      .x
    }
  })

  # Format headers as bold, and include label attributes (if any)
  labels <- purrr::map(colnames(.df), ~ attr(.df[[.x]], "label"))
  names_with_labels <- purrr::map2_chr(colnames(.df), labels, ~{
    sub_txt <- if(!is.null(.y)) paste0("<br>", "<i>", .y, "</i>") else ""
    paste0("<i>", .x, "</b>", sub_txt)
  })

  # Return the .df table
  .df_view <- DT::datatable(
    .df,
    colnames = names_with_labels,
    rownames = FALSE,
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    selection = "none",
    # style = "bootstrap4",
    # class = 'compact cell-border',
    class = 'cell-border hover order-column nowrap stripe',
    # class = 'table-bordered table-hover nowrap',
    # class = 'cell-border nowrap',
    extensions = c("RowGroup", "Buttons", "ColReorder", "FixedColumns"),
    plugins = 'natural',
    options = tableOpts
  ) %>%
    DT::formatStyle(0:ncol(.df), target = "cell", border = '0.5px solid #ddd', padding= "1px") %>%
    DT::formatStyle(0, target= 'row',lineHeight='85%') %>%
    DT::formatStyle(columns = colnames(.df), fontSize = '80%')


  if(.view == "window"){
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(.df_view, html_file, selfcontained = TRUE)
    browseURL(html_file)
  }else{
    .df_view
  }

}


