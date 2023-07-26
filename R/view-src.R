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
#' @importFrom dplyr across n
#'
#' @export
view_src <- function(
    .df,
    .subject_col = NULL,
    .view = c("viewer", "window"),
    .full_width = FALSE
){

  .view <- match.arg(.view)

  # Dont autofit width for few number of columns
  # otherwise DT will not align the headers properly (known open bug)
  autoWidth <- if(ncol(.df) <= 6) FALSE else TRUE


  # Determine widths
  if(isTRUE(.full_width)){
    col_width <- paste0(100/ncol(.df), "%")
  }else{
    col_width <- "30px"
  }

  base_font_size <- 9

  # Table options
  tableOpts = list(
    dom = "B<lf<\"datatables-scroll\"t>ipr>",
    pageLength = 100,
    lengthMenu = c(25,100,500,1000),
    headerCallback = DT::JS(
      "function(thead) {",
      glue("$(thead).css('font-size', '{base_font_size+3}pt');"),
      "}"
    ),
    scrolly = "15vh",
    scrollX = TRUE,
    scrollY = 800,
    scrollCollapse=TRUE,
    colReorder = TRUE,
    searchHighlight = TRUE,
    autoWidth = autoWidth,
    select = list(style = 'os', items = 'row')
  )

  columnDefs <- list(
    list(className = 'dt-center', targets = "_all"),
    list(width = col_width, targets = "_all"),
    # Controls column ordering
    list(type = 'natural', targets = "_all")
  )

  # If no .subject_col specified, look for USUBJID and ID
  if(is.null(.subject_col)){
    id_cols <- grepl("^(?i)ID$|^(?i)USUBJID$", names(.df))
    if(any(id_cols)) .subject_col <- names(.df)[id_cols]
  }

  # Fix .subject_col column
  if (!is.null(.subject_col)) {
    stopifnot(.subject_col %in% names(.df))

    .df <- .df %>% dplyr::relocate(!!!syms(.subject_col))
    fix_col_index <- purrr::map_dbl(.subject_col, ~ grep(paste0("^",.x, "$"), names(.df)))
    # TODO: change this to color grouping
    # tableOpts$rowGroup <- list(dataSrc = fix_col_index - 1)
    tableOpts$fixedColumns <- list(leftColumns = max(fix_col_index))

    # Add color column
    .df <- .df %>%
      dplyr::group_by(dplyr::across(all_of(.subject_col))) %>%
      dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
      dplyr::mutate(color = ifelse(group_id %% 2 == 1, "white", "#e2e2e2")) %>%
      dplyr::ungroup() %>% dplyr::select(-group_id)

    # Hides color column in output
    color_id <- grep("color", names(.df)) - 1
    columnDefs <- c(columnDefs, list(list(targets = color_id, visible = F)))
  }

  # Assign columnDefs to table options
  tableOpts$columnDefs <- columnDefs

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
    sub_txt <- if(!is.null(.y)){
      .y <- paste0("(", .y, ")")
      paste0("<br>", glue("<i><span style='color: #8A8B8C; font-size: {base_font_size+1.5}pt'>"), .y, "</i></span> ")
    }else{
      ""
    }
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
    class = 'cell-border hover order-column nowrap stripe',
    extensions = c("RowGroup", "ColReorder", "FixedColumns"),
    plugins = 'natural',
    options = tableOpts
  ) %>%
    DT::formatStyle(0:ncol(.df), target = "cell", border = '0.5px solid #bbbbbb', padding= "1px") %>%
    DT::formatStyle(0, target= 'row',lineHeight='85%') %>%
    DT::formatStyle(0:ncol(.df), fontSize = paste0(base_font_size, "pt")) %>%
    DT::formatStyle(0:ncol(.df), "color",
                    backgroundColor = DT::styleEqual(sort(unique(.df$color)), sort(unique(.df$color)))
    )


  if(.view == "window"){
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(.df_view, html_file, selfcontained = TRUE)
    browseURL(html_file)
  }else{
    .df_view
  }

}


