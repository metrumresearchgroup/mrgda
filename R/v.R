#' create_datatable function
#'
#' This function modifies the input dataframe by converting columns with fewer than 20 unique values to factors.
#' It adds labels to the column names and sets up a datatable with a range of options. If the specified
#' subject column is present, it is used to group rows in the table.
#'
#' @param .df A dataframe that you want to process and view.
#' @param .subject_col A character string specifying the subject column. If this column is
#' present in the dataframe, it will be used to group rows in the datatable.
#' @param .freeze_cols A character vector specifying columns to freeze when scrolling horizontally.
#' `.subject_col` will automatically be appended to this list (i.e. `.subject_col` is always frozen).
#' @param .digits number of digits to round numeric columns to.
#'
#' @details
#' If `.subject_col` is `NULL`, the column names `"USUBJID"` and `"ID"` will be searched and set if present.
#' If `.subject_col` is present in the dataframe, it will be frozen and alternate in color.
#'
#' Columns with less than 20 unique values are converted to factors. This can be helpful for filtering, which
#' will show up as the shiny equivalent of `selectInput`, as opposed to the default `textInput` for character columns.
#'
#' Columns with label attributes will be appended to column headers as a new line.
#'
#' You can *drag and drop* columns to move them around in the table. If a column is dragged to a frozen column's location,
#' the new column will be frozen instead.
#'
#' @return An instance of `DT::datatable` class with the processed dataframe.
#'
#' @importFrom dplyr across n
#' @importFrom tidyselect where
#'
#' @export
v <- function(
    .df,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3
){


  if(interactive() && object.size(.df) > 2.1e6){
    size <- format(as.numeric(object.size(.df)), scientific = TRUE, digits = 4)
    msg <- c(
      "x" = glue(".df object size is {size}, which must be less than 2.1e6 to render on the client side."),
      "i"= "Use `mrgda::src_viz(list(.df))` for large datasets, which renders the table using your R console"
    )
    cli::cli_abort(msg)
  }

  # Dont autofit width for few number of columns
  # otherwise DT will not align the headers properly (known open bug)
  autoWidth <- if(ncol(.df) <= 6) FALSE else TRUE

  # Set basic options
  col_width <- "1px"
  base_font_size <- 10

  # Table options
  tableOpts = list(
    dom = "B<lf<\"datatables-scroll\"t>ipr>",
    pageLength = 100,
    lengthMenu = c(25,100,500,1000),
    headerCallback = DT::JS(
      "function(thead) {",
      glue("$(thead).css('font-size', '{base_font_size-0.5}pt');"),
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

  .freeze_cols <- c(.subject_col, .freeze_cols)

  # Fix .freeze_cols and color coat .subject_col
  if (!is.null(.freeze_cols)) {
    stopifnot(.freeze_cols %in% names(.df))

    .df <- .df %>% dplyr::relocate(!!!syms(.freeze_cols))
    fix_col_index <- purrr::map_dbl(.freeze_cols, ~ grep(paste0("^",.x, "$"), names(.df)))
    tableOpts$fixedColumns <- list(leftColumns = max(fix_col_index))

    if (!is.null(.subject_col)) {
      stopifnot(.subject_col %in% names(.df))

      # Add color column
      .df <- .df %>%
        dplyr::group_by(dplyr::across(all_of(.subject_col))) %>%
        dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
        dplyr::mutate(color = ifelse(.data$group_id %% 2 == 1, "white", "#ececec")) %>%
        dplyr::ungroup() %>% dplyr::select(-"group_id")
    }
  }

  if(!("color" %in% names(.df))) .df <- .df %>% dplyr::mutate(color = "white")

  caption <- make_v_caption(.df, .subject_col, base_font_size + 2)

  # Hides color column in output
  color_id <- grep("color", names(.df)) - 1
  columnDefs <- c(columnDefs, list(list(targets = color_id, visible = F)))

  # Assign columnDefs to table options
  tableOpts$columnDefs <- columnDefs


  # Format headers as bold, and include column attributes (label and class)
  names_with_labels <- format_v_headers(.df)

  # Round numeric columns to 3 decimal places
  .df <- .df %>% dplyr::mutate(across(where(is.numeric),\(x) prettyNum2(x, .digits)))

  # Convert columns with less than 20 unique values to factors
  .df <- purrr::map_dfr(.df, ~ {
    if(length(unique(.x)) < 20){
      as.factor(.x)
    } else {
      .x
    }
  })

  # Return the .df table
  .df_view <- DT::datatable(
    .df,
    caption = caption,
    colnames = names_with_labels,
    rownames = FALSE,
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    selection = "none",
    class = 'cell-border hover order-column nowrap',
    extensions = c("RowGroup", "ColReorder", "FixedColumns"),
    plugins = 'natural',
    options = tableOpts
  ) %>%
    DT::formatStyle(0:ncol(.df), target = "cell", border = '1px solid #bbbbbb', padding= "1px") %>%
    DT::formatStyle(0, target= 'row',lineHeight='85%') %>%
    DT::formatStyle(0:ncol(.df), fontSize = paste0(base_font_size, "pt")) %>%
    DT::formatStyle(0:ncol(.df), "color",
                    backgroundColor = DT::styleEqual(sort(unique(.df$color)), sort(unique(.df$color)))
    ) %>% suppressWarnings()

  if(interactive()){
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(.df_view, html_file)
    rstudioapi::viewer(html_file, height = "maximize")
  }
  return(invisible(.df_view))
}


