#' Format headers to include column attributes (label and class)
#'
#' @inheritParams v
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
