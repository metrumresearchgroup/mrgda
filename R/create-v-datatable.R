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
#' @param .digits number of digits to round numeric columns to. Set to `NULL` to prevent rounding.
#' @param dt_options list of options for formatting the `DT::datatable`
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
#' @importFrom utils object.size
#'
#' @keywords internal
create_v_datatable <- function(
    .df,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3,
    dt_options = list(
      show_filters = FALSE,
      show_labels = TRUE,
      wrap_labels = FALSE,
      trunc_labels = 20,
      ft_size = 9,
      subj_contrast = FALSE
    )
){


  # This shouldnt be triggered by users, but is a precautionary abort call to avoid
  # crashing an R session when attempting to render large datasets on the client side.
  if(rlang::is_interactive() && object.size(.df) > 2.1e6){
    size <- format(as.numeric(object.size(.df)), scientific = TRUE, digits = 4)
    msg <- c(
      "x" = glue(".df object size is {size}, which must be less than 2.1e6 to render on the client side."),
      "i"= "Use `mrgda::src_viz(list(.df))` for large datasets, which renders the table using your R console"
    )
    cli::cli_abort(msg)
  }

  # Dont autofit width for few number of columns
  # otherwise DT will not align the headers properly (known open bug)
  autoWidth <- if(ncol(.df) <= 8) FALSE else TRUE

  # Set basic options
  col_width <- "1px"
  base_font_size <- dt_options$ft_size

  # Table options
  tableOpts = list(
    dom = "B<\"datatables-scroll\"t>irf<\"row\">lp",
    pageLength = 100,
    lengthMenu = c(100,500,1000,5000),
    headerCallback = DT::JS(
      "function(thead) {",
      glue("$(thead).css('font-size', '{base_font_size-0.5}pt');"),
      "}"
    ),
    scrolly = "15vh",
    scrollX = TRUE,
    scrollY = 425,
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
    list(type = 'natural', targets = "_all"),
    # Show NAs (to differentiate between blank and NA)
    list(targets = "_all", render = DT::JS(
      "function(data, type, row, meta) {",
      "return data === null ? 'NA' : data;",
      "}")
    )
  )

  # If no .subject_col specified, look for USUBJID and ID (choose first in appearance)
  if(is.null(.subject_col)){
    id_cols <- grepl("^(?i)ID$|^(?i)USUBJID$", names(.df))
    if(any(id_cols)) .subject_col <- names(.df)[id_cols]
    if(length(.subject_col) > 1) .subject_col <- .subject_col[1]
  }else{
    assertthat::assert_that(length(.subject_col) == 1)
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

      # Set subject alternation color
      color_rot <- ifelse(isTRUE(dt_options$subj_contrast), "yellow", "#ececec")

      # Add group columns (color, bg color, and borders)
      .df <- .df %>%
        dplyr::group_by(dplyr::across(all_of(.subject_col))) %>%
        dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
        dplyr::mutate(
          bg_color = ifelse(.data$group_id %% 2 == 1, "white", color_rot),
          ft_color = ifelse(.data$group_id %% 2 == 1, "black", "black"),
          subj_border = ifelse(dplyr::row_number() == dplyr::last(dplyr::row_number()), 1, 0)
        ) %>% # was #ececec
        dplyr::ungroup() %>% dplyr::select(-"group_id")
    }
  }

  if(!("bg_color" %in% names(.df))){
    .df <- .df %>% dplyr::mutate(bg_color = "white", ft_color = "black")
  }


  # Hides grouping columns in output
  color_id <- grep("bg_color|ft_color|subj_border", names(.df)) - 1
  columnDefs <- c(columnDefs, list(list(targets = color_id, visible = F)))

  # Assign columnDefs to table options
  tableOpts$columnDefs <- columnDefs


  # Format headers as bold, and include column attributes (label and class)
  names_with_labels <- format_v_headers(.df,
                                        .show_labels = dt_options$show_labels,
                                        .wrap_labels = dt_options$wrap_labels,
                                        .trunc_labels = dt_options$trunc_labels,
                                        .trunc_length = dt_options$trunc_length)

  # Round numeric columns to 3 decimal places
  if(!is.null(.digits)){
    .df <- .df %>% dplyr::mutate(across(where(is.numeric), ~prettyNum2(.x, .digits)))
  }

  # Convert columns with less than 20 unique values to factors
  .df <- purrr::map_dfr(.df, ~ {
    if(length(unique(.x)) < 20){
      as.factor(.x)
    } else {
      .x
    }
  })


  # Escape special characters that could be misunderstood as HTML
  # TODO: see if we can apply the opposite approach to headers instead
  # This is unlikely, as many HTML escaping mechanisms are designed to prevent
  # the rendering of HTML tags and entities for security reasons
  .df <- .df %>% dplyr::mutate(across(everything(), ~ htmltools::htmlEscape(.x)))

  # User controlled table options
  filter <- ifelse(dt_options$show_filters, "top", "none")

  # Return the .df table
  .df_view <- DT::datatable(
    .df,
    colnames = names_with_labels,
    rownames = FALSE,
    escape = FALSE,
    filter = filter,
    selection = "none",
    class = 'cell-border hover order-column nowrap',
    extensions = c("RowGroup", "ColReorder", "FixedColumns"),
    plugins = 'natural',
    options = tableOpts
  ) %>%
    #
    # Column Borders
    DT::formatStyle(0:ncol(.df), target = "cell", border = '1px solid #bbbbbb', padding= "2px") %>%
    # Line Height
    DT::formatStyle(0, target= 'row',lineHeight='85%') %>%
    # Font Size
    DT::formatStyle(0:ncol(.df), fontSize = paste0(base_font_size, "pt")) %>%
    # Background color
    DT::formatStyle(
      0:ncol(.df), "bg_color",
      backgroundColor = DT::styleEqual(sort(unique(.df$bg_color)), sort(unique(.df$bg_color)))
    ) %>%
    # Font color
    DT::formatStyle(
      0:ncol(.df), "ft_color",
      color = DT::styleEqual(sort(unique(.df$ft_color)), sort(unique(.df$ft_color)))
    ) %>%
    suppressWarnings()

  # Column Borders for fixed columns
  if(!is.null(tableOpts$fixedColumns)){
    .df_view <- .df_view %>%
      DT::formatStyle(
        tableOpts$fixedColumns$leftColumns, target = "cell",
        `border-right` = '3px solid #007319', padding= "1px"
      )
  }

  # Row Borders for .subject_col column
  if(!is.null(.subject_col)){
    .df_view <- .df_view %>%
      DT::formatStyle(
        0:ncol(.df), "subj_border",
        `border-bottom` = DT::styleEqual(1,'2px solid black')
      )
  }

  return(.df_view)
}


