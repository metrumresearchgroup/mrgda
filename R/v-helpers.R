
# Helpers for create_v_datatable ------------------------------------------


#' Format headers to include column attributes in create_v_datatable (label and class)
#'
#' @inheritParams create_v_datatable
#' @param .wrap_labels Logical (T/F). IF `TRUE`, wrap labels to 20 characters, and indent new lines
#' @param .show_labels Logical (T/F). IF `TRUE`, show label attributes under the column name
#'
#' @keywords internal
format_v_headers <- function(.df, .wrap_labels = TRUE, .show_labels = TRUE){
  col_attributes <- purrr::map(colnames(.df), ~ {
    list(col_lbl = attr(.df[[.x]], "label"), col_class = readr::guess_parser(as.character(.df[[.x]])))
  })

  names_with_labels <- purrr::map2_chr(colnames(.df), col_attributes, function(col_name, col_attr){
    # Label attributes
    lbl_sub_txt <- if(!is.null(col_attr$col_lbl)){
      col_lbl <- if(isTRUE(.wrap_labels)){
        paste0(col_attr$col_lbl) %>%
          stringr::str_trunc(20) %>%
          gsub(" ", "<br>", ., fixed = TRUE) %>%
          gsub("_", "<br>", ., fixed = TRUE)
      }else{
        paste0(col_attr$col_lbl)
      }
      paste0("<br>", glue("<span style='color: #8A8B8C;'>"), col_lbl, "</span>")
    }else{
      ""
    }

    # Rename class
    class_name <- dplyr::case_when(
      col_attr$col_class == "character" ~ "<chr>",
      col_attr$col_class == "double" ~ "<dbl>",
      col_attr$col_class == "datetime" ~ "<dttm>",
      col_attr$col_class == "logical" ~ "<lgl>",
      TRUE ~ paste0("<", col_attr$col_class, ">")
    ) %>% gsub(">", "&gt;", .) %>% gsub("<", "&lt;", .)

    # Define color based on class
    class_color <- dplyr::case_when(
      col_attr$col_class == "character" ~ "#D63A33",
      col_attr$col_class == "double" ~ "#6BA93C",
      col_attr$col_class == "logical" ~ "#d3b747",
      col_attr$col_class %in% c("datetime", "time", "date") ~ "#3366A4",
      TRUE ~ "black"
    )

    class_sub_text <- paste0("<br>", glue("<i><span style='color: {class_color};'>"),
                             class_name, "</span></i>")

    # Create overall header
    final_label <- if(isTRUE(.show_labels)){
      paste0(col_name, lbl_sub_txt, class_sub_text)
    }else{
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
    .x >= 1,
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
setup_v_list <- function(.df_list){
  if(!inherits(.df_list, "list")){
    if(!(inherits(.df_list, "data.frame") || inherits(.df_list, "tbl_df"))){
      cli::cli_abort(".df_list must be a list, data.frame, or tibble")
    }
    .df_list <- list(.df_list)
  }

  # Ensure list elements are named
  if(is.null(names(.df_list))){
    names(.df_list) <- paste("Dataframe", seq(length(.df_list)))
  }

  # Filter out mrgda specific dataframe
  .df_list <- .df_list[!grepl("mrgda", names(.df_list), fixed = TRUE)]

  return(.df_list)
}



#' Search for ID columns across list of dataframes
#'
#' Search for the specified ID columns across list of dataframes, and return the column that appears the most.
#' Returns `NULL` if none are found.
#'
#' @inheritParams v
#' @param .id_cols vector of id columns to search for
#'
#' @importFrom tidyselect all_of
#'
#' @returns a character string
#'
#' @keywords internal
check_subject_col <- function(.df_list, .id_cols = c("USUBJID", "ID")){
  # Look for USUBJID and ID columns across datasets
  id_col_df <- purrr::map_dfr(.df_list, function(df){
    purrr::map_lgl(.id_cols, function(id_col){
      any(grepl(glue("^(?i){id_col}$"), names(df)))
    }) %>% stats::setNames(.id_cols)
  }) %>% dplyr::mutate(dataset = names(.df_list)) %>%
    tidyr::pivot_longer(all_of(.id_cols), names_to = "id_col", values_to = "present")

  if(!any(id_col_df$present)){
    # Set to NULL if none are found
    subject_col <- NULL
  }else{
    # Use the id_col with the most occurrences across the datasets
    id_count <- id_col_df %>% dplyr::filter(.data$present) %>%
      dplyr::count(.data$id_col, .data$present)
    subject_col <- id_count$id_col[id_count$n == max(id_count$n)]
    # Use USUBJID if they are the same frequency
    if(length(subject_col) > 1){
      subject_col <- .id_cols[1]
    }
  }

  return(subject_col)
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
make_v_caption <- function(.name, .df, .subject_col = NULL, .font_size = 10){

  # Calculate N subjects
  if(!is.null(.subject_col)){
    num_subj <- .df %>% dplyr::count(!!!syms(.subject_col)) %>% nrow()
    n_subj_txt <- glue('(N {.subject_col}: {num_subj})')
  }else{
    num_subj <- 0
    n_subj_txt <- 'No subjects detected'
  }

  # Format dataframe name
  name_style <- glue('text-align: center; font-size:{.font_size}pt; font-weight: bold; display: block;')
  name_html <- htmltools::tags$span(
    style = name_style,
    .name
  )

  # Format subject count
  n_subj_style <- glue('text-align: center; font-size:{.font_size-2}pt; display: block;')
  n_subj_html <- htmltools::tags$span(
    style = n_subj_style,
    n_subj_txt
  )

  # Combine and format as HTML
  caption <- paste(.name, n_subj_txt)
  html_caption <- paste0(name_html, n_subj_html)
  html_caption <- as.character(shiny::tags$div(htmltools::HTML(html_caption)))

  tibble::tibble(name = .name, n_subs = num_subj,
                 label = caption, html_label = html_caption)
}


#' Create global filter for the src_viz shiny app
#'
#' global filter is embedded in the title of a `shinydashboard::box`
#'
#' @inheritParams src_viz
#'
#' @importFrom shiny HTML
#'
#' @returns a `shiny.tag` object
#'
#' @keywords internal
create_global_filter <- function(.subject_col){
  if(!is.null(.subject_col)){
    global_filter_ui <-
      fluidRow(
        column(
          width = 12, style = "margin-top: 7px;",
          htmltools::tags$style("div.form-group {margin-bottom: 0px;}"),
          shiny::textInput(inputId = "subject_filter", label = NULL,
                           placeholder = paste("Global", .subject_col, "Filter"))
        )
      )

  }else{
    global_filter_ui <- shiny::div()
  }

  return(global_filter_ui)
}

