#' View a formatted dataframe in the viewer
#'
#' Creates a DT::datatable object and runs it in a background shiny application in the Rstudio viewer pane.
#'
#' @param .df_list A dataframe *or* list of dataframes that you want to process and view.
#' @inheritParams create_v_datatable
#'
#' @details
#'
#' This function modifies the input dataframe by converting columns with fewer than 20 unique values to factors.
#' It adds labels to the column names and sets up a datatable with a range of options. If the specified
#' subject column is present, it is used to group rows in the table.
#'
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
#'
#' @export
v <- function(
    .df_list,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3
){

  args <- list(
    .df_list = .df_list,
    .subject_col = .subject_col,
    .freeze_cols = .freeze_cols,
    .digits = .digits
  )

  run_app_bg(v_shiny_internal, args = args)
}



#' Basic shiny app for running `mrgda::create_v_datatable()` on the server
#'
#' @inheritParams v
#' @inheritParams create_v_datatable
#' @inheritParams run_app_bg
#'
#' @importFrom htmltools tags
#'
#' @keywords internal
v_shiny_internal <- function(
    .df_list,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3,
    host = NULL,
    port = NULL
){

  # For Development Environment
  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    message("Loading ", load_path)
    devtools::load_all(load_path)
  }

  if(!inherits(.df_list, "list")){
    .df_list <- list(.df_list)
  }

  # Ensure list elements are named
  if(is.null(names(.df_list))){
    names(.df_list) <- paste("Dataframe", seq(length(.df_list)))
  }

  # Filter out mrgda specific dataframe
  .df_list <- .df_list[!grepl("mrgda", names(.df_list), fixed = TRUE)]

  # Determine .subject_col if not specified
  if(is.null(.subject_col)){
    .subject_col <- check_subject_col(.df_list)
  }else{
    # Make sure .subject_col is valid
    if(!any(purrr::map_lgl(.df_list, ~{.subject_col %in% names(.df_list)}))){
      abort(glue(".subject_col ({.subject_col}) is not present in any dataframe"))
    }
  }

  # Create global filter UI
  global_filter_ui <- create_global_filter(.subject_col)

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = NULL, disable = TRUE),
    shinydashboard::dashboardSidebar(disable = TRUE),
    shinydashboard::dashboardBody(
      shinydashboard::box(
        width = 12,
        title = tags$span(global_filter_ui),
        status = "primary",
        solidHeader = TRUE,
        v_ui("df_view", .df_list, .subject_col)
      )
    )
  )


  server <- function(input, output, session) {

    subject_filter <- reactive(input$subject_filter)
    v_server("df_view", .df_list, .subject_col, .freeze_cols, .digits,
             subject_filter = subject_filter)

    # End the process on window close. This is designed for the case where a
    # single user launches the app privately with run_app_bg(). If this app is
    # ever reworked into a multi-user setting, this should be removed.
    #
    # Note that a refresh will also trigger this.
    session$onSessionEnded(shiny::stopApp)
  }

  app <- shinyApp(ui = ui, server = server, options = list(host = host, port = port))
  shiny::runApp(app)
}




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

