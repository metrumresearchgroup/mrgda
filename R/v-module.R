

#' shiny module ui for `create_v_datatable`
#'
#' @param id shiny module id. Character string
#' @inheritParams v
#' @importFrom htmltools tagList
#' @importFrom shiny NS fluidRow column
#'
#' @keywords internal
v_ui <- function(id, .df_list, .subject_col){

  ns <- NS(id)

  tagList(
    # tags$style(".dataTables_scrollBody {max-height: 400px !important;}"),
    # Make a tab for every domain
    do.call(
      shinydashboard::tabBox,
      c(
        purrr::map2(names(.df_list), .df_list, function(.name, .df){
          caption <- make_v_caption(.df, .subject_col)
          shiny::tabPanel(
            title = .name,
            shiny::tags$br(),
            shiny::tags$div(caption),
            shiny::tags$br(),
            fluidRow(
              column(
                width = 12, align = "center",
                DT::DTOutput(ns(paste0("table", .name)))
              )
            )
          )
        }),
        list(width = 12)
      )
    )
  )
}

#' shiny module server for `create_v_datatable`
#' @inheritParams v
#' @inheritParams create_v_datatable
#' @inheritParams v_ui
#' @param subject_filter reactive expression pointing to global filter
#' @importFrom shiny moduleServer shinyApp
#'
#' @keywords internal
v_server <- function(
    id,
    .df_list,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3,
    subject_filter
){

  moduleServer(id, function(input, output, session) {

    purrr::map2(names(.df_list), .df_list, function(.name, .df) {
      output[[paste0("table", .name)]] <- DT::renderDT({

        has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(.df)
        do_rows_filter <- has_subject_col && nchar(trimws(subject_filter())) > 0

        if(do_rows_filter){
          .df_filter <- .df %>% dplyr::filter(grepl(trimws(subject_filter()), !!sym(.subject_col)))
        }else{
          .df_filter <- .df
        }

        # Fix .freeze_cols found per dataset, so that it still works with lists
        if(!is.null(.freeze_cols)){
          .freeze_cols_df <- .freeze_cols[.freeze_cols %in% names(.df)]
          if(rlang::is_empty(.freeze_cols_df)) .freeze_cols_df <- NULL
        }else{
          .freeze_cols_df <- NULL
        }

        # Allows for some dataframes to not have .subject_col
        if(has_subject_col){
          if(nrow(.df_filter) == 0){
            .df_filter[1, .subject_col] <- "<b>No subjects found</b>"
          }
          create_v_datatable(.df_filter, .subject_col, .freeze_cols_df, .digits)
        }else{
          create_v_datatable(.df, .subject_col = NULL, .freeze_cols_df, .digits)
        }
      }, server = TRUE)
    })

  })
}


