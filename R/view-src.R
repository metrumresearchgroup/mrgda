#' create_datatable function
#'
#' This function modifies the input dataframe by converting columns with fewer than 20 unique values to factors.
#' It adds labels to the column names and sets up a datatable with a range of options. If the specified
#' subject column is present, it is used to group rows in the table.
#'
#' @param .df A dataframe that you want to process and view.
#' @param .subject_cols A character vector specifying the subject columns. If these columns are
#' present in the dataframe, it will be used to group rows in the datatable.
#' @param .freeze_cols A character vector specifying columns to freeze when scrolling horizontally.
#' `.subject_cols` will automatically be appended to this list (i.e. `.subject_cols` are always frozen).
#' @param .group_id Logical (`TRUE`/`FALSE`). If `TRUE`, add breaks between each unique `.subject_cols` grouping.
#'
#' @details
#' If `.subject_cols` is `NULL`, the column names `"USUBJID"` and `"ID"` will be searched and set if present.
#' If `.subject_cols` are present in the dataframe, they will be frozen and alternate in color.
#'
#' Columns with less than 20 unique values are converted to factors. This can be helpful for filtering, which
#' will show up as the shiny equivalent of `selectInput`, as opposed to the default `textInput` for character columns.
#'
#' Columns with label attributes will be appended to column headers as a new line.
#'
#' You can *drag and drop* columns to move them around in the table. If a column is dragged to a frozen column's location,
#' the new column will be frozen instead.
#'
#' @return An instance of DT::datatable class with the processed dataframe.
#'
#' @importFrom dplyr across n
#'
#' @export
v <- function(
    .df,
    .subject_cols = NULL,
    .freeze_cols = NULL,
    .group_id = FALSE
){


  # Dont autofit width for few number of columns
  # otherwise DT will not align the headers properly (known open bug)
  autoWidth <- if(ncol(.df) <= 6) FALSE else TRUE

  # Set basic options
  col_width <- "30px"
  base_font_size <- 9

  # Table options
  tableOpts = list(
    dom = "B<lf<\"datatables-scroll\"t>ipr>",
    pageLength = 100,
    lengthMenu = c(25,100,500,1000),
    headerCallback = DT::JS(
      "function(thead) {",
      glue("$(thead).css('font-size', '{base_font_size+1}pt');"),
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

  # If no .subject_cols specified, look for USUBJID and ID
  if(is.null(.subject_cols)){
    id_cols <- grepl("^(?i)ID$|^(?i)USUBJID$", names(.df))
    if(any(id_cols)) .subject_cols <- names(.df)[id_cols]
  }

  .freeze_cols <- c(.subject_cols, .freeze_cols)

  # Fix .subject_cols column
  if (!is.null(.freeze_cols)) {
    stopifnot(.freeze_cols %in% names(.df))

    .df <- .df %>% dplyr::relocate(!!!syms(.freeze_cols))
    fix_col_index <- purrr::map_dbl(.freeze_cols, ~ grep(paste0("^",.x, "$"), names(.df)))
    tableOpts$fixedColumns <- list(leftColumns = max(fix_col_index))

    if (!is.null(.subject_cols)) {
      stopifnot(.subject_cols %in% names(.df))

      # Optionally break by .subject_cols
      if(isTRUE(.group_id)){
        tableOpts$rowGroup <- list(dataSrc = fix_col_index - 1)
      }

      # Add color column
      .df <- .df %>%
        dplyr::group_by(dplyr::across(all_of(.subject_cols))) %>%
        dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
        dplyr::mutate(color = ifelse(.data$group_id %% 2 == 1, "white", "#ececec")) %>%
        dplyr::ungroup() %>% dplyr::select(-"group_id")
    }
  }

  if(!("color" %in% names(.df))) .df <- .df %>% dplyr::mutate(color = "white")

  # Hides color column in output
  color_id <- grep("color", names(.df)) - 1
  columnDefs <- c(columnDefs, list(list(targets = color_id, visible = F)))

  # Assign columnDefs to table options
  tableOpts$columnDefs <- columnDefs


  # Format headers as bold, and include column attributes (label and class)
  names_with_labels <- format_v_headers(.df, base_font_size)


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
    colnames = names_with_labels,
    rownames = FALSE,
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    selection = "none",
    # style = "bootstrap4",
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
    )


}


# needed for gsub piping
utils::globalVariables(c("."))

#' Format headers as bold, and include column attributes (label and class)
#'
#' @inheritParams v
#' @param base_font_size font size used to format the header
#'
#' @keywords internal
format_v_headers <- function(.df, base_font_size){
  col_attributes <- purrr::map(colnames(.df), ~ {
    list(col_lbl = attr(.df[[.x]], "label"), col_class = readr::guess_parser(as.character(.df[[.x]])))
  })

  names_with_labels <- purrr::map2_chr(colnames(.df), col_attributes, function(col_name, col_attr){
    # Label attributes
    lbl_sub_txt <- if(!is.null(col_attr$col_lbl)){
      col_attr$col_lbl <- paste0("(", col_attr$col_lbl, ")")
      paste0("<br>", glue("<span style='color: #8A8B8C; font-size: {base_font_size}pt'>"), col_attr$col_lbl, "</span>")
    }else{
      ""
    }

    # Rename class
    class_name <- dplyr::case_when(
      col_attr$col_class == "character" ~ "<chr>",
      col_attr$col_class == "double" ~ "<dbl>",
      col_attr$col_class == "datetime" ~ "<dttm>",
      TRUE ~ col_attr$col_class
    ) %>% gsub(">", "&gt;", .) %>% gsub("<", "&lt;", .)

    # Define color based on class
    class_color <- dplyr::case_when(
      col_attr$col_class == "character" ~ "#D63A33",
      col_attr$col_class == "double" ~ "#6BA93C",
      col_attr$col_class == "datetime" ~ "#3366A4",
      TRUE ~ "black"
    )

    class_sub_text <- paste0("<br>", glue("<i><span style='color: {class_color}; font-size: {base_font_size}pt'>"),
                             class_name, "</span></i>")

    # Create overall header
    paste0("<b>", col_name, "</b>", lbl_sub_txt, class_sub_text)
  })

  return(names_with_labels)
}


