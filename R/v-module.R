#' shiny module ui for `create_v_datatable`
#'
#' @param id shiny module id. Character string
#' @importFrom htmltools tagList
#' @importFrom shiny NS fluidRow column
#'
#' @keywords internal
v_ui <- function(id){

  ns <- NS(id)

  tagList(
    # tags$style(".dataTables_scrollBody {max-height: 400px !important;}"),
    fluidRow(
      column(
        width = 12, align = "center",
        DT::DTOutput(ns("df_view"))
      )
    )
  )
}

#' shiny module server for `create_v_datatable`
#'
#' @inheritParams create_v_datatable
#' @inheritParams v_ui
#' @importFrom shiny moduleServer shinyApp
#'
#' @keywords internal
v_server <- function(
    id,
    .df,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3
){

  moduleServer(id, function(input, output, session) {

    output$df_view <- DT::renderDT({
      create_v_datatable(.df, .subject_col, .freeze_cols, .digits)
    }, server = TRUE)

  })
}

