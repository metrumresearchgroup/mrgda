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
