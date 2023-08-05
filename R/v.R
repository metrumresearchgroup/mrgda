#' Format and view a dataframe or list of dataframes in the viewer
#'
#' Creates a `DT::datatable` object for each dataframe and runs it in a background shiny application in the Rstudio viewer pane.
#'
#' @param .df_list A dataframe ***or*** list of dataframes that you want to process and view.
#' @inheritParams create_v_datatable
#'
#' @details
#'
#' **For each dataframe in `.df_list`:**
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
#' @note
#' **Notes about the background process:**
#'
#' By running the app as a background process, users will retain access to their console.
#' The process will end when one of the following things happen:
#'  - the Rstudio viewer pane process is stopped
#'  - another `v()` call is executed (only one will run at a time)
#'  - you manually kill it via the steps below:
#'    ```
#'    result <- v(mtcars)
#'    result$kill()
#'
#'    result
#'    PROCESS 'R', finished.
#'    ```
#'
#' @examples
#' \dontrun{
#'
#'  df_list <- mrgda::read_src_dir(system.file("example-sdtm", package = "mrgda"))
#'
#'  # "USUBJID" automatically detected as `.subject_col`
#'
#'  v(df_list)
#'
#'  # manually specify `.subject_col`
#'
#'  v(df_list, .subject_col = "USUBJID")
#'
#'  # `.freeze_cols` found **per** dataset will be fixed while horizontally scrolling
#'  # If a specified `.freeze_cols` is not found in one of the datasets, it will simply be ignored.
#'
#'  v(df_list, .freeze_cols = c("STUDYID", "RFSTDTC"))
#' }
#'
#' @return The `callr` process (invisibly).
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



#' Create shiny app for running `mrgda::create_v_datatable()` on the server
#'
#' Create and execute a basic shiny app for running `mrgda::create_v_datatable()` on the server.
#' Renders a `DT::datatable` object for each dataframe in a `shiny::tabPanel` object.
#'
#'
#' @inheritParams v
#' @inheritParams create_v_datatable
#' @inheritParams run_app_bg
#'
#' @importFrom htmltools tags
#'
#' @return a `shinyApp` object
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

  # For Development Environment, must load the package
  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    message("Loading ", load_path)
    devtools::load_all(load_path)
  }

  .df_list <- setup_v_list(.df_list)

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

    onStop(function(){
      cli::cli_inform(c("i"="Session Stopped\n"))
    })

    # Global subject filter
    subject_filter <- reactive(input$subject_filter)

    # Create DT datatables
    v_server("df_view", .df_list, .subject_col, .freeze_cols, .digits,
             .subject_filter = subject_filter)

    # End the process on window close. This is designed for the case where a
    # single user launches the app privately with run_app_bg(). If this app is
    # ever reworked into a multi-user setting, this should be removed.
    #
    # Note that a refresh will also trigger this.
    session$onSessionEnded(shiny::stopApp)
  }

  # Messages wont be shown in the console when run in the background, but can be
  # recovered before or after the process is finished.
  onStart <- function() {
    shiny::onStop(function() {
      cli::cli_inform(c("i"="Recovering memory\n"))
      gc()
    })
  }

  app <- shinyApp(ui = ui, server = server, onStart = onStart,
                  options = list(host = host, port = port))
  shiny::runApp(app)
}




#' shiny module UI for `v_shiny_internal`
#'
#' @inheritParams v_server
#' @inheritParams create_v_datatable
#'
#' @importFrom htmltools tagList
#' @importFrom shiny NS fluidRow column
#'
#' @returns a shiny module UI object
#'
#' @keywords internal
v_ui <- function(.id, .df_list, .subject_col){

  ns <- NS(.id)

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

#' shiny module server for `v_shiny_internal`
#'
#' @inheritParams create_v_datatable
#' @param .id shiny module id. Character string
#' @param .df_list a list of dataframes
#' @param .subject_filter reactive expression pointing to global filter
#'
#' @importFrom shiny moduleServer shinyApp
#'
#' @returns a shiny module server object
#'
#' @keywords internal
v_server <- function(
    .id,
    .df_list,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3,
    .subject_filter
){

  moduleServer(.id, function(input, output, session) {

    purrr::map2(names(.df_list), .df_list, function(.name, .df) {
      output[[paste0("table", .name)]] <- DT::renderDT({

        has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(.df)
        do_rows_filter <- has_subject_col && nchar(trimws(.subject_filter())) > 0

        if(do_rows_filter){
          .df_filter <- .df %>% dplyr::filter(grepl(trimws(.subject_filter()), !!sym(.subject_col)))
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

