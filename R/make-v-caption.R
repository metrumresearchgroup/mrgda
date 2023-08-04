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
