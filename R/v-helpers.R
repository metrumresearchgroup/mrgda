
# Helpers for create_v_datatable ------------------------------------------


#' Format headers to include column attributes in create_v_datatable (label and class)
#'
#' @inheritParams create_v_datatable
#'
#' @keywords internal
format_v_headers <- function(.df){
  col_attributes <- purrr::map(colnames(.df), ~ {
    list(col_lbl = attr(.df[[.x]], "label"), col_class = readr::guess_parser(as.character(.df[[.x]])))
  })

  names_with_labels <- purrr::map2_chr(colnames(.df), col_attributes, function(col_name, col_attr){
    # Label attributes
    lbl_sub_txt <- if(!is.null(col_attr$col_lbl)){
      col_lbl <- paste0(col_attr$col_lbl) %>%
        gsub(" ", "<br>", ., fixed = TRUE) %>%
        gsub("_", "<br>", ., fixed = TRUE) %>%
        gsub(".", "<br>", ., fixed = TRUE)
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
    paste0(col_name, lbl_sub_txt, class_sub_text)
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


#' Search for ID columns across list of dataframes
#'
#' @inheritParams src_viz
#' @param id_cols vector of id columns to search for
#'
#' @importFrom tidyselect all_of
#'
#' @keywords internal
check_subject_col <- function(.src_list, id_cols = c("ID", "USUBJID")){
  # Look for USUBJID and ID columns across datasets
  id_col_df <- purrr::map_dfr(.src_list, function(df){
    purrr::map_lgl(id_cols, function(id_col){
      any(grepl(glue("^(?i){id_col}$"), names(df)))
    }) %>% stats::setNames(id_cols)
  }) %>% dplyr::mutate(dataset = names(.src_list)) %>%
    tidyr::pivot_longer(all_of(id_cols), names_to = "id_col", values_to = "present")

  if(!any(id_col_df$present)){
    # Set to NULL if none are found
    subject_col <- NULL
  }else{
    # Use the id_col with the most occurrences across the datasets
    id_count <- id_col_df %>% dplyr::count(.data$id_col, .data$present)
    subject_col <- id_count$id_col[id_count$n == max(id_count$n)]
    # Use USUBJID if they are the same frequency
    if(length(subject_col) > 1){
      subject_col <- "USUBJID"
    }
  }

  return(subject_col)
}


#' Make the caption for `create_v_datatable()`, returning the number of subjects
#'
#' @inheritParams create_v_datatable
#' @param font_size font size of the caption
#'
#' @keywords internal
make_v_caption <- function(.df, .subject_col, font_size = 12){
  style <- glue('text-align: left; color:#FFFFFF; background-color:#5A5A5A; font-size:{font_size}pt;
                font-weight: bold; padding: 3px')

  cap_txt <- if(!is.null(.subject_col)){
    num_subj <- .df %>% dplyr::count(!!!syms(.subject_col)) %>% nrow()
    glue('N Subjects ({.subject_col}): {num_subj}')
  }else{
    'No subjects detected'
  }

  htmltools::tags$span(
    style = style,
    cap_txt
  )

}


#' Create global filter for the src_viz shiny app
#'
#' @inheritParams src_viz
#'
#' @keywords internal
create_global_filter <- function(.subject_col){
  if(!is.null(.subject_col)){
    global_filter_ui <-
      fluidRow(
        column(5,
               shiny::div(style = "font-size:18px;", paste0("Global ", .subject_col, " Filter"))
        ),
        column(6,
               htmltools::tags$style("div.form-group {margin-bottom: 0px;}"),
               shiny::textInput(inputId = "subject_filter", label = NULL)
        )
      )

  }else{
    global_filter_ui <- shiny::div()
  }

  return(global_filter_ui)
}

