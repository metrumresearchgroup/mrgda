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
#' @importFrom shinyWidgets tooltipOptions dropdownButton pickerInput
#'
#' @details
#' It's easier to use this function for testing during development, as messages
#' will be printed to the console.
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

  table_opts <- purrr::map2_dfr(names(.df_list), .df_list, function(.name, .df){
    make_v_caption(.name, .df, .subject_col)
  })

  ui <- shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = NULL, disable = TRUE),
    shinydashboardPlus::dashboardSidebar(width = 0),
    shinydashboard::dashboardBody(
      tags$head(
        tags$style(".content{padding-bottom: 0px !important;
                   padding-top: 0px !important;}")
      ),
      fluidRow(
        style = "background-color: #007319; color: white;",
        column(
          width = 4,
          pickerInput(
            inputId = "data_view", label = "Selected:",
            inline = TRUE, width = "fit",
            choices = table_opts$name,
            choicesOpt = list(content = table_opts$html_label),
            options = list(style = "btn-success")
          )
        ),
        column(
          width = 4, offset = 3,
          global_filter_ui
        ),
        column(
          width = 1,  align = "right",
          style = "padding: 0px;",
          dropdownButton(
            circle = FALSE, status = "success", right = TRUE, size = "lg",
            icon = shiny::icon("gear"), tooltip = tooltipOptions(title = "Table Options", placement = "left"),
            shinyWidgets::checkboxGroupButtons(
              "dt_options", label = htmltools::h4("Table Options", style = "color:black;"),
              individual = TRUE, direction = "vertical",
              choiceNames = c("Show column filters", "Wrap column labels", "Show column labels"),
              choiceValues = c("show_filters", "wrap_labels", "show_labels"),
              selected = c("wrap_labels", "show_labels"),
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
            )
          )
        )
      ),
      fluidRow(
        class = "main_body",
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

    # Selected Dataset
    data_view <- reactive({
      .df_list[[shiny::req(input$data_view)]]
    })

    # DT options
    dt_options <- reactive({
      list(
        show_filters = "show_filters" %in% input$dt_options,
        wrap_labels = "wrap_labels" %in% input$dt_options,
        show_labels = "show_labels" %in% input$dt_options
      )
    })

    # Create DT datatables
    v_server("df_view", .df = data_view, .subject_col, .freeze_cols, .digits,
             .subject_filter = subject_filter, .dt_options = dt_options)

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
#' @importFrom htmltools tagList br
#' @importFrom shiny NS fluidRow column
#'
#' @returns a shiny module UI object
#'
#' @keywords internal
v_ui <- function(.id, .df_list, .subject_col){

  ns <- NS(.id)

  tagList(
    shinydashboard::box(
      width = NULL, title = NULL,
      fluidRow(
        column(
          width = 12, align = "center",
          DT::DTOutput(ns("df_view"))
        )
      )
    )
  )
}

#' shiny module server for `v_shiny_internal`
#'
#' @param .id shiny module id. Character string
#' @inheritParams create_v_datatable
#' @param .subject_filter reactive expression pointing to global filter
#'
#' @importFrom shiny moduleServer shinyApp
#'
#' @returns a shiny module server object
#'
#' @keywords internal
v_server <- function(
    .id,
    .df,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3,
    .subject_filter,
    .dt_options
){

  moduleServer(.id, function(input, output, session) {

    output$df_view <- DT::renderDT({

      has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(.df())
      do_rows_filter <- has_subject_col && nchar(trimws(.subject_filter())) > 0

      if(do_rows_filter){
        .df_filter <- .df() %>% dplyr::filter(grepl(trimws(.subject_filter()), !!sym(.subject_col)))
      }else{
        .df_filter <- .df()
      }

      # Fix .freeze_cols found per dataset, so that it still works with lists
      if(!is.null(.freeze_cols)){
        .freeze_cols_df <- .freeze_cols[.freeze_cols %in% names(.df())]
        if(rlang::is_empty(.freeze_cols_df)) .freeze_cols_df <- NULL
      }else{
        .freeze_cols_df <- NULL
      }

      # Allows for some dataframes to not have .subject_col
      if(has_subject_col){
        if(nrow(.df_filter) == 0){
          .df_filter[1, .subject_col] <- "<b>No subjects found</b>"
        }
        create_v_datatable(.df_filter, .subject_col, .freeze_cols_df, .digits,
                           dt_options = .dt_options())
      }else{
        create_v_datatable(.df(), .subject_col = NULL, .freeze_cols_df, .digits,
                           dt_options = .dt_options())
      }
    }, server = TRUE)

  })
}

