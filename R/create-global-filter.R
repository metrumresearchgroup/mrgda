#' Create global filter for the src_viz shiny app
#'
#' @inheritParams src_viz
#'
#' @keywords internal
create_global_filter <- function(.subject_col){
  if(!is.null(.subject_col)){
    global_filter_ui <-
      shiny::textInput(
        inputId = "subject_filter",
        label = paste0("Global ", .subject_col, " Filter"),
        width = "100%"
      )

  }else{
    global_filter_ui <- shiny::div()
  }

  return(global_filter_ui)
}
