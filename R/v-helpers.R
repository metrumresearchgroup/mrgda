# create_v_datatable (main DT function) -----------------------------------


#' Creates a formatted `DT::datatable`
#'
#' Creates a formatted `DT::datatable` with many adjustable formatting options. Designed to be called within
#'  `v()` or `v_shiny_internal` for easier formatting adjustments and server-side processing.
#'
#' @param .df A dataframe that you want to process and view.
#' @param .subject_col A character string specifying the subject column. If this column is
#' present in the dataframe, it will be used to group rows in the datatable.
#' @param .freeze_cols A character vector specifying columns to freeze when scrolling horizontally.
#' `.subject_col` will automatically be appended to this list (i.e. `.subject_col` is always frozen).
#' @param .digits number of digits to round numeric columns to. Set to `NULL` to prevent rounding.
#' @param .show_filters Logical (`TRUE`/`FALSE`). If `TRUE`, show column filters.
#' @param .show_labels Logical (`TRUE`/`FALSE`). If `TRUE`, show label attributes associated with the columns.
#' @param .wrap_labels Logical (`TRUE`/`FALSE`). If `TRUE`, wrap column labels to have multuple lines.
#' @param .trunc_labels Logical (`TRUE`/`FALSE`). If `TRUE`, truncate the column label attributes.
#' @param .trunc_length Numeric. Maximum width of column labels. Only relevant if `.trunc_labels = TRUE`.
#' @param .ft_size Numeric. Base font size of the table. Other labels scale off this value.
#' @param .scroll_y Numeric. Height of the table in pixels. Passed to `scrollY` option of `DT::datatable`.
#' @param .subj_contrast Logical (`TRUE`/`FALSE`). If `TRUE`, increase the color contrast between unique `.subject_col` groupings.
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
    .show_filters = FALSE,
    .show_labels = TRUE,
    .wrap_labels = FALSE,
    .trunc_labels = FALSE,
    .trunc_length = 20,
    .ft_size = 9,
    .scroll_y = 400,
    .subj_contrast = FALSE) {
  # This shouldnt be triggered by users, but is a precautionary abort call to avoid
  # crashing an R session when attempting to render large datasets on the client side.
  if (rlang::is_interactive() && object.size(.df) > 2.1e6) {
    size <- format(as.numeric(object.size(.df)), scientific = TRUE, digits = 4)
    msg <- c(
      "x" = glue(".df object size is {size}, which must be less than 2.1e6 to render on the client side."),
      "i" = "Use `mrgda::v(.df)` for large datasets, which renders the table using your R console"
    )
    cli::cli_abort(msg)
  }

  # Dont autofit width for few number of columns
  # otherwise DT will not align the headers properly (known open bug)
  autoWidth <- if (ncol(.df) <= 8) FALSE else TRUE

  # Set basic options
  col_width <- "1px"
  base_font_size <- .ft_size

  # Core table options
  tableOpts <- list(
    dom = "<'dt-wrapper'<'datatables-scroll'rt><'row-pad' lf>ip>",
    pageLength = 100,
    lengthMenu = c(25, 100, 500, 1000, 5000),
    scrollX = TRUE,
    scrollY = .scroll_y,
    scrollCollapse = TRUE,
    colReorder = TRUE,
    searchHighlight = TRUE,
    autoWidth = autoWidth,
    select = list(style = "os", items = "row")
  )


  # Adjust font size and formatting of header on init
  tableOpts$initComplete <- DT::JS(
    "function(settings, json) {",
    "  var mainHeader = $(this.api().table().header());",
    glue("mainHeader.css('font-size', '{base_font_size}pt');"),
    "  var indivHeader = mainHeader.find('th');",
    "  indivHeader.css('padding-top', '0px');",
    "  indivHeader.css('padding-bottom', '1px');",
    "  indivHeader.css('border-bottom', '2px solid #007319');",
    "}"
  )

  # Add a callback to improve margins of DT info elements
  tableOpts$drawCallback <- DT::JS(
    "function(settings) {",
    "  var mainBody = $('.dt-wrapper');",
    "  var rowPad = mainBody.find('.row-pad');",
    "  rowPad.css('padding-top', '1em');",
    "}"
  )

  columnDefs <- list(
    list(className = "dt-center", targets = "_all"),
    list(width = col_width, targets = "_all"),
    # Controls column ordering
    list(type = "natural", targets = "_all"),
    # Show NAs (to differentiate between blank and NA)
    list(targets = "_all", render = DT::JS(
      "function(data, type, row, meta) {",
      "return data === null ? 'NA' : data;",
      "}"
    ))
  )

  # If no .subject_col specified, look for USUBJID and ID (choose first in appearance)
  if (is.null(.subject_col)) {
    id_cols <- grepl("^(?i)ID$|^(?i)USUBJID$", names(.df))
    if (any(id_cols)) .subject_col <- names(.df)[id_cols]
    if (length(.subject_col) > 1) .subject_col <- .subject_col[1]
  } else {
    assertthat::assert_that(length(.subject_col) == 1)
  }

  .freeze_cols <- c(.subject_col, .freeze_cols)

  # Fix .freeze_cols and color coat .subject_col
  if (!is.null(.freeze_cols)) {
    stopifnot(.freeze_cols %in% names(.df))

    .df <- .df %>% dplyr::relocate(!!!syms(.freeze_cols))
    fix_col_index <- purrr::map_dbl(.freeze_cols, ~ grep(paste0("^", .x, "$"), names(.df)))
    tableOpts$fixedColumns <- list(leftColumns = max(fix_col_index))

    if (!is.null(.subject_col)) {
      stopifnot(.subject_col %in% names(.df))

      # Set subject alternation color
      color_rot <- ifelse(isTRUE(.subj_contrast), "yellow", "#ececec")

      # Add group columns (color, bg color, and borders)
      .df <- .df %>%
        dplyr::group_by(dplyr::across(all_of(.subject_col))) %>%
        dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
        dplyr::mutate(
          bg_color = ifelse(.data$group_id %% 2 == 1, "white", color_rot),
          ft_color = ifelse(.data$group_id %% 2 == 1, "black", "black"),
          subj_border = ifelse(dplyr::row_number() == dplyr::last(dplyr::row_number()), 1, 0)
        ) %>% # was #ececec
        dplyr::ungroup() %>%
        dplyr::select(-"group_id")
    }
  }

  if (!("bg_color" %in% names(.df))) {
    .df <- .df %>% dplyr::mutate(bg_color = "white", ft_color = "black")
  }


  # Hides grouping columns in output
  color_id <- grep("bg_color|ft_color|subj_border", names(.df)) - 1
  columnDefs <- c(columnDefs, list(list(targets = color_id, visible = F)))

  # Assign columnDefs to table options
  tableOpts$columnDefs <- columnDefs


  # Format headers as bold, and include column attributes (label and class)
  names_with_labels <- format_v_headers(.df,
    .font_size = base_font_size - 1,
    .show_labels = .show_labels,
    .wrap_labels = .wrap_labels,
    .trunc_labels = .trunc_labels,
    .trunc_length = .trunc_length
  )

  # Round numeric columns to 3 decimal places
  if (!is.null(.digits)) {
    .df <- .df %>% dplyr::mutate(across(where(is.numeric), ~ prettyNum2(.x, .digits)))
  }


  # Escape special characters that could be misunderstood as HTML
  # (this must happen before factor conversion)
  # TODO: see if we can apply the opposite approach to headers instead
  # This is unlikely, as many HTML escaping mechanisms are designed to prevent
  # the rendering of HTML tags and entities for security reasons
  .df <- .df %>% dplyr::mutate(across(where(is.character), ~ htmltools::htmlEscape(.x)))


  # Convert columns with less than 20 unique values to factors
  .df <- purrr::map_dfr(.df, ~ {
    if (length(unique(.x)) < 20) {
      as.factor(.x)
    } else {
      .x
    }
  })


  # User controlled table options
  filter <- if (.show_filters) list(position = "top", clear = FALSE) else "none"

  # Determine lineheight based on font size
  # EQ developed from 40% = 5pt & 100% = 12pt (lm(c(40, 100) ~ c(5, 12)))
  line_height <- paste0(round(8.6 * base_font_size - 2.9), "%")

  # Return the .df table
  .df_view <- DT::datatable(
    .df,
    colnames = names_with_labels,
    rownames = FALSE,
    escape = FALSE,
    filter = filter,
    selection = "none",
    class = "cell-border hover order-column nowrap",
    extensions = c("RowGroup", "ColReorder", "FixedColumns"),
    plugins = "natural",
    options = tableOpts
  ) %>%
    # Column Borders
    DT::formatStyle(0:ncol(.df), target = "cell", border = "1px solid #bbbbbb", padding = "2px") %>%
    # Line Height
    DT::formatStyle(0, target = "row", lineHeight = line_height) %>%
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
  if (!is.null(tableOpts$fixedColumns)) {
    .df_view <- .df_view %>%
      DT::formatStyle(
        tableOpts$fixedColumns$leftColumns,
        target = "cell",
        `border-right` = "2px solid #007319", padding = "1px"
      )
  }

  return(.df_view)
}




# Helpers for create_v_datatable ------------------------------------------


#' Format headers to include column attributes in create_v_datatable (label and class)
#'
#' @inheritParams create_v_datatable
#' @param .font_size Numeric. Base font size (in pt) of the column labels.
#' @param .show_labels Logical (T/F). IF `TRUE`, show label attributes under the column name.
#' @param .wrap_labels Logical (T/F). IF `TRUE`, wrap labels to 20 characters, and indent new lines.
#' @param .trunc_labels Logical (T/F). IF `TRUE`, truncate labels.
#' @param .trunc_length Numeric. Number of characters to truncate labels to. Only relevant if `.trunc_labels` is `TRUE`.
#'
#' @keywords internal
format_v_headers <- function(
    .df,
    .font_size = 8,
    .show_labels = TRUE,
    .wrap_labels = FALSE,
    .trunc_labels = TRUE,
    .trunc_length = 20) {
  col_attributes <- purrr::map(colnames(.df), ~ {
    list(col_lbl = attr(.df[[.x]], "label"), col_class = readr::guess_parser(as.character(.df[[.x]])))
  })

  names_with_labels <- purrr::map2_chr(colnames(.df), col_attributes, function(col_name, col_attr) {
    # Label attributes
    lbl_sub_txt <- if (!is.null(col_attr$col_lbl)) {
      col_lbl <- paste0(col_attr$col_lbl)
      if (isTRUE(.trunc_labels)) {
        assertthat::is.number(.trunc_length)
        col_lbl <- col_lbl %>% stringr::str_trunc(.trunc_length)
      }
      if (isTRUE(.wrap_labels)) {
        col_lbl <- col_lbl %>%
          gsub(" ", "<br>", ., fixed = TRUE) %>%
          gsub("_", "<br>", ., fixed = TRUE)
      }
      lbl_style <- glue("<span style='color: #8A8B8C; font-size: {.font_size-0.25}pt;
                        display:block; margin-top:1px; line-height:95%;'>")
      paste0(lbl_style, col_lbl, "</span>")
    } else {
      ""
    }

    # Rename class
    class_name <- dplyr::case_when(
      col_attr$col_class == "character" ~ "<chr>",
      col_attr$col_class == "double" ~ "<dbl>",
      col_attr$col_class == "datetime" ~ "<dttm>",
      col_attr$col_class == "logical" ~ "<lgl>",
      TRUE ~ paste0("<", col_attr$col_class, ">")
    ) %>%
      gsub(">", "&gt;", .) %>%
      gsub("<", "&lt;", .)

    # Define color based on class
    class_color <- dplyr::case_when(
      col_attr$col_class == "character" ~ "#D63A33",
      col_attr$col_class == "double" ~ "#6BA93C",
      col_attr$col_class == "logical" ~ "#d3b747",
      col_attr$col_class %in% c("datetime", "time", "date") ~ "#3366A4",
      TRUE ~ "black"
    )
    class_name <- paste0("<i>", class_name, "</i>")
    class_style <- glue("<span style='color: {class_color}; font-size: {.font_size+0.5}pt;
                        display: block;'>")
    class_sub_text <- paste0(class_style, class_name, "</span>")

    # Create overall header
    col_name_style <- glue("<h4 style = 'font-size: {.font_size+1}pt; line-height:0%; font-weight: bold;'>")
    col_name <- paste0(col_name_style, col_name, "</h4>")
    final_label <- if (isTRUE(.show_labels)) {
      paste0(col_name, lbl_sub_txt, class_sub_text)
    } else {
      paste0(col_name, class_sub_text)
    }
  })

  return(names_with_labels)
}


#' Round numeric value with significant digits
#'
#' @param .x numeric value
#' @param .digits number of digits to round numeric value to
#'
#' @return numeric value
#'
#' @keywords internal
prettyNum2 <- function(.x, .digits = 3) {
  ifelse(
    .x >= 1 | .x <= -1,
    round(.x, digits = .digits),
    signif(.x, digits = .digits)
  )
}


# Helpers for v_shiny_internal and v --------------------------------------


#' Put `.df_list` in the right format
#'
#' @inheritParams v
#'
#' @return list of dataframes
#'
#' @keywords internal
setup_v_list <- function(.df_list, .subject_col) {
  if (!inherits(.df_list, "list")) {
    if (!(inherits(.df_list, "data.frame") || inherits(.df_list, "tbl_df"))) {
      cli::cli_abort(".df_list must be a list, data.frame, or tibble")
    }
    .df_list <- list(.df_list)
  }

  # Ensure list elements are named
  if (is.null(names(.df_list))) {
    names(.df_list) <- paste("Dataframe", seq(length(.df_list)))
  }

  # Filter out mrgda specific dataframe
  .df_list <- .df_list[!grepl("mrgda", names(.df_list), fixed = TRUE)]

  # Determine subject column and freeze columns
  v_cols <- gather_v_cols(.df_list, .subject_col)

  # Put in format for run_app_bg
  args <- c(list(.df_list = .df_list), v_cols)

  return(args)
}




#' Determine subject column and which columns can be frozen
#'
#' Search for the specified ID columns across list of dataframes, and return the
#' column that appears the most as the subject column (if not specified).
#' The freeze columns will be all columns outside of the determined subject column
#'
#' @inheritParams v
#' @param .id_cols vector of id columns to search for
#'
#' @importFrom tidyselect all_of
#'
#' @returns a named list of subject column and available freeze columns
#'
#' @keywords internal
gather_v_cols <- function(
    .df_list,
    .subject_col = NULL,
    .id_cols = c("USUBJID", "ID")) {
  if (!is.null(.subject_col)) {
    # Make sure .subject_col is valid if user specified
    if (!any(purrr::map_lgl(.df_list, ~ {
      .subject_col %in% colnames(.x)
    }))) {
      abort(glue(".subject_col ({.subject_col}) is not present in any dataframe"))
    }
    subject_col <- .subject_col
  } else {
    # Look for USUBJID and ID columns across datasets
    id_col_df <- purrr::map_dfr(.df_list, function(df) {
      purrr::map_lgl(.id_cols, function(id_col) {
        any(grepl(glue("^(?i){id_col}$"), names(df)))
      }) %>% stats::setNames(.id_cols)
    }) %>%
      dplyr::mutate(dataset = names(.df_list)) %>%
      tidyr::pivot_longer(all_of(.id_cols), names_to = "id_col", values_to = "present")

    if (!any(id_col_df$present)) {
      # Set to NULL if none are found
      subject_col <- NULL
    } else {
      # Use the id_col with the most occurrences across the datasets
      id_count <- id_col_df %>%
        dplyr::filter(.data$present) %>%
        dplyr::count(.data$id_col, .data$present)
      subject_col <- id_count$id_col[id_count$n == max(id_count$n)]
      # Use USUBJID if they are the same frequency
      if (length(subject_col) > 1) {
        subject_col <- .id_cols[1]
      }
    }
  }

  # Make sure subject_col is part of .id_cols (if user specified)
  id_cols <- unique(c(.id_cols, subject_col))

  # Freeze column choices
  unique_cols <- names(table(unlist(purrr::map(.df_list, ~ names(.x)))))
  freeze_cols <- unique_cols[!(unique_cols %in% id_cols)]
  freeze_cols <- purrr::map_dfr(freeze_cols, find_df_with_col, .df_list) %>%
    dplyr::arrange(dplyr::desc(.data$n_domain))


  if (!is.null(subject_col)) {
    cli::cli_alert_info(glue::glue("Detected Subject Column: ", subject_col))
  }

  return(
    list(
      .subject_col = subject_col,
      .freeze_cols = freeze_cols
    )
  )
}


#' Search list of dataframes for column
#'
#' @param .col_name column name
#' @inheritParams v
#'
#' @keywords internal
#'
#' @returns tibble
find_df_with_col <- function(.col_name, .df_list) {
  df_with_col <- character(0)
  for (i in seq_along(.df_list)) {
    if (.col_name %in% colnames(.df_list[[i]])) {
      df_with_col <- c(df_with_col, names(.df_list)[i])
    }
  }

  max_elements <- 2 # Max domains to display
  n_domains <- length(df_with_col) # N domains found

  if (n_domains > max_elements) {
    sub_txt <- paste0(df_with_col[1:max_elements], collapse = ", ")
    sub_txt <- paste(sub_txt, "and", n_domains - max_elements, "more")
  } else {
    sub_txt <- paste0(df_with_col, collapse = ", ")
  }

  tibble::tibble(
    col_name = .col_name,
    n_domain = n_domains,
    subtitle = paste0("(", sub_txt, ")")
  )
}

#' Make a caption displaying the dataset name and number of subjects
#'
#' Make a caption for for each rendering of `create_v_datatable()`, displaying the
#' number of subjects under the dataset name (center aligned).
#'
#' @param .name name of the dataset
#' @inheritParams create_v_datatable
#' @param .font_size font size of the caption
#'
#' @returns a `shiny.tag` object
#'
#' @keywords internal
make_v_caption <- function(.name, .df, .subject_col = NULL, .font_size = 10) {
  # Calculate N subjects
  if (!is.null(.subject_col)) {
    num_subj <- .df %>%
      dplyr::count(!!!syms(.subject_col)) %>%
      nrow()
    n_subj_txt <- glue("(N {.subject_col}: {num_subj})")
  } else {
    num_subj <- 0
    n_subj_txt <- "No subjects detected"
  }

  # Format dataframe name
  name_style <- glue("text-align: center; font-size:{.font_size}pt; font-weight: bold; display: block;")
  name_html <- htmltools::tags$span(
    style = name_style,
    .name
  )

  # Format subject count
  n_subj_style <- glue("text-align: center; font-size:{.font_size-2}pt; display: block;")
  n_subj_html <- htmltools::tags$span(
    style = n_subj_style,
    n_subj_txt
  )

  # Combine and format as HTML
  caption <- paste(.name, n_subj_txt)
  html_caption <- paste0(name_html, n_subj_html)
  html_caption <- as.character(shiny::tags$div(htmltools::HTML(html_caption)))

  tibble::tibble(
    name = .name, n_subs = num_subj,
    label = caption, html_label = html_caption
  )
}


#' Create global filter for the v shiny app
#'
#' global filter is embedded in the title of a `shinydashboard::box`
#'
#' @inheritParams v
#'
#'
#' @returns a `shiny.tag` object
#'
#' @keywords internal
create_global_filter <- function(.subject_col) {
  if (!is.null(.subject_col)) {
    global_filter_ui <-
      shiny::fluidRow(
        shiny::column(
          width = 12, style = "margin-top: 7px;",
          htmltools::tags$style("div.form-group {margin-bottom: 0px;}"),
          shiny::textInput(
            inputId = "subject_filter", label = NULL,
            placeholder = paste(.subject_col, "Filter")
          )
        )
      )
  } else {
    global_filter_ui <- shiny::div()
  }

  return(global_filter_ui)
}


#' Filter dataframe by subject column
#'
#' @inheritParams create_v_datatable
#' @param .subject_filter a string to seach for within the defined `.subject_col`
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' filter_v_subject(df, "USUBJID", "3053-4")
#' }
#'
#' @return a filtered dataframe
filter_v_subject <- function(.df, .subject_col = NULL, .subject_filter) {
  has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(.df)
  do_rows_filter <- has_subject_col && nchar(trimws(.subject_filter)) > 0

  if (do_rows_filter) {
    data_filter <- .df %>% dplyr::filter(grepl(trimws(.subject_filter), !!sym(.subject_col)))
  } else {
    data_filter <- .df
  }

  # Allows for some dataframes to not have .subject_col
  if (has_subject_col) {
    if (nrow(data_filter) == 0) {
      data_filter[1, .subject_col] <- "No subjects found"
    }
  }

  return(data_filter)
}

#' Checks if all packages needed for v() are present
#'
#' Returns a vector with the missing packages, or returns NULL if all are
#' present.
#' @keywords internal
check_for_v_pkgs <- function() {
  pkgs_present <- purrr::map_lgl(REQUIRED_V_PKGS, function(.pkg) {
    requireNamespace(.pkg, quietly = TRUE)
  })

  if (any(!pkgs_present)) {
    return(REQUIRED_V_PKGS[!pkgs_present])
  } else {
    return(NULL)
  }
}

#' Skip tests if missing v() dependencies
#' @keywords internal
skip_if_v_missing_deps <- function() {
  missing_pkgs <- check_for_v_pkgs()
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("Skipped because the following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )
}

#' Error if missing v() dependencies
#' @keywords internal
stop_if_v_missing_deps <- function() {
  missing_pkgs <- check_for_v_pkgs()
  if (!is.null(missing_pkgs)) {
    rlang::abort(paste( # need to use rlang::abort instead of stop to get the pkgr formatting to print correctly
      glue::glue("The following packages needed to run `v()` are not installed: {paste(missing_pkgs, collapse = ', ')}"),
      "Consider running `install.packages('mrgda', dependencies = TRUE)`",
      "\nIf using pkgr, add the following to your pkgr.yml and re-run `pkgr install`",
      "\nCustomizations:\n  Packages:\n    - mrgda:\n        Suggests: true",
      sep = "\n"
    ), call. = FALSE)
  }
}
