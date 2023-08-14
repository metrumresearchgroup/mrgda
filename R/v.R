#' Format and view a dataframe or list of dataframes in the viewer
#'
#' Creates a `DT::datatable` object for each dataframe and runs it in a background shiny application in the Rstudio viewer pane.
#'
#' @param .df_list A dataframe ***or*** list of dataframes that you want to process and view.
#' Named lists will inherit their names when shown in the app.
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
    .subject_col = NULL
){

  args <- setup_v_list(.df_list, .subject_col)

  run_app_bg(v_shiny_internal, args = args)
}



#' Create shiny app for running `mrgda::create_v_datatable()` on the server
#'
#' Create and execute a basic shiny app for running `mrgda::create_v_datatable()` on the server.
#' Renders a `DT::datatable` object for each dataframe in a `shiny::tabPanel` object.
#'
#'
#' @inheritParams v
#' @param .freeze_cols A tibble of freeze columns. One column containing the column names,
#' and the other with the domains they were found in.
#' @inheritParams run_app_bg
#' @param dont_run Logical (`TRUE`/`FALSE`). If `TRUE`, return the shinyApp object
#'  instead of executing it via `runApp`. Used for testing
#'
#' @importFrom htmltools tags
#' @importFrom shiny onStop
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
    host = NULL,
    port = NULL,
    dont_run = FALSE
){

  # For Development Environment, must load the package
  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    message("Loading ", load_path)
    devtools::load_all(load_path)
  }

  # Only triggers if v_shiny_internal is called directly
  if(rlang::is_interactive()){
    args <- setup_v_list(.df_list, .subject_col)
    .subject_col <- args$.subject_col
    .freeze_cols <- args$.freeze_cols
    .df_list <- args$.df_list
  }


  ui <- shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = NULL, disable = TRUE),
    shinydashboardPlus::dashboardSidebar(width = 0),
    shinydashboard::dashboardBody(
      tags$head(
        # Remove whitespace
        tags$style(".content{padding-bottom: 0px !important;
                   padding-top: 0px !important;}"),
        # PickerInput styling (hard to see before - columns stand out more)
        tags$style(HTML(".dropdown-menu > li > a {font-size: smaller;}")),
        tags$style(HTML(".dropdown-header {font-size: larger; font-weight: bold;}")),
        tags$style(HTML(".dropdown-menu > .divider {background-color: black; height: 2px;}")),
        # Ensures minimum width of pickerInput
        tags$style(HTML(".open > .dropdown-menu {width: 250px !important;}")),
        # Other Styling
        tags$style(HTML("hr {border-top: 2px solid #007319;}"))
      ),
      # Main Header UIs
      v_global_ui(.df_list, .subject_col, .freeze_cols),
      # Individual Dataframe
      fluidRow(v_mod_ui("df_view"))
    )
  )


  server <- function(input, output, session) {

    onStop(function(){
      cli::cli_inform(c("i"="Session Stopped\n"))
    })

    # Handle global UIs
    global_vars <- v_global_server(.df_list, input, output)

    # Create DT datatables
    v_mod_server("df_view",
                 .subject_col = .subject_col,
                 global_vars = reactive(global_vars)
    )

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

  if(isTRUE(dont_run)){
    return(app)
  }

  shiny::runApp(app)
}




#' shiny module UI for `v_shiny_internal`
#'
#' @inheritParams v_mod_server
#'
#' @importFrom htmltools tagList br
#' @importFrom shiny NS fluidRow column
#'
#' @returns a shiny module UI object
#'
#' @keywords internal
v_mod_ui <- function(.id){

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
#' @param global_vars reactive list of arguments
#' @inheritParams create_v_datatable
#'
#' @importFrom shiny moduleServer shinyApp
#'
#' @returns a shiny module server object
#'
#' @keywords internal
v_mod_server <- function(
    .id,
    .subject_col = NULL,
    global_vars
){

  moduleServer(.id, function(input, output, session) {


    # Reactive display data
    data <- reactive({
      data_raw <- shiny::req(global_vars()$data_select)
      subject_filter <- global_vars()$subject_filter

      has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(data_raw)
      do_rows_filter <- has_subject_col && nchar(trimws(subject_filter)) > 0

      if(do_rows_filter){
        data_filter <- data_raw %>% dplyr::filter(grepl(trimws(subject_filter), !!sym(.subject_col)))
      }else{
        data_filter <- data_raw
      }

      # Allows for some dataframes to not have .subject_col
      if(has_subject_col){
        if(nrow(data_filter) == 0){
          data_filter[1, .subject_col] <- "No subjects found"
        }
      }
      data_filter
    })


    # Set subject column per dataset (to account for list elements where subject_col is missing)
    subject_col_df <- reactive({
      if(!is.null(.subject_col) && .subject_col %in% names(data())){
        .subject_col
      }else{
        NULL
      }
    })

    # Fix .freeze_cols found per dataset, so that it still works with lists
    freeze_cols <- reactive({
      fix_cols <- global_vars()$freeze_col_choices
      if(!is.null(fix_cols)){
        .freeze_cols_df <- fix_cols[fix_cols %in% names(data())]
        if(rlang::is_empty(.freeze_cols_df)) .freeze_cols_df <- NULL
      }else{
        .freeze_cols_df <- NULL
      }
      .freeze_cols_df
    })

    output$df_view <- DT::renderDT({
      args_df <- list(
        .df = shiny::req(data()),
        .subject_col = subject_col_df(),
        .freeze_cols = freeze_cols()
      )

      args <- c(args_df, global_vars()$dt_args)

      # Dont consider shiny session to be interactive (avoid object size check)
      rlang::with_interactive({
        do.call(create_v_datatable, args)
      }, value = FALSE)
    }, server = TRUE)

  })
}


#' Construct global header UI for `v_shiny_internal`
#'
#' Serves as a wrapper for all global UIs
#'
#' @details
#' Note that this is not module
#'
#' @inheritParams v
#' @inheritParams v_shiny_internal
#'
#' @return a shiny UI
#'
#' @keywords internal
v_global_ui <- function(.df_list, .subject_col, .freeze_cols){

  # Create global filter UI
  global_filter_ui <- create_global_filter(.subject_col)

  # For dataframe selection (formats names and N subjects)
  table_opts <- purrr::map2_dfr(names(.df_list), .df_list, function(.name, .df){
    make_v_caption(.name, .df, .subject_col)
  })

  fluidRow(
    style = "background-color: #007319; color: white;",
    column(
      width = 3, align = "left",
      pickerInput(
        inputId = "data_select", label = "Data:",
        inline = TRUE, width = "fit",
        choices = table_opts$name,
        choicesOpt = list(content = table_opts$html_label),
        options = list(style = "btn-success")
      )
    ),
    column(
      width = 3, align = "left",
      style = "margin-top: 7px;",
      shinyWidgets::pickerInput(
        "freeze_cols", label = NULL,
        choices = .freeze_cols$col_name,
        options = list(
          `live-search` = TRUE, `actions-box` = TRUE,
          style = "btn-success", size = 8,
          title = "Freeze columns"
        ),
        choicesOpt = list(subtext = .freeze_cols$subtitle),
        multiple = TRUE
      )
    ),
    column(
      width = 2, align = "left",
      style = "margin-top: 14px;",
      shinyWidgets::materialSwitch(
        "show_filters", label = "Filters",
        right = TRUE, value = FALSE, status = "success"
      )
    ),
    column(
      width = 3, align = "right",
      style = "padding-left: 0px;",
      global_filter_ui
    ),
    column(
      width = 1, align = "right",
      style = "margin-top: 5px;",
      shinyWidgets::dropMenu(
        shiny::actionButton(
          "table_opts", label = NULL,
          icon = shiny::icon("gear", style = "color: white"),
          style='font-size:120%',
          class = "btn-success"),
        tags$div(
          style = "text-align: left;",
          shinyWidgets::awesomeCheckboxGroup(
            "dt_options", label = htmltools::h4("Column Labels"),
            choices = c("Show labels" = "show_labels",
                        "Wrap labels" = "wrap_labels",
                        "Truncate labels" = "trunc_labels"),
            selected = c("show_labels")
          ),
          shiny::conditionalPanel(
            condition = "input.dt_options.indexOf('trunc_labels') != -1",
            tags$br(),
            fluidRow(
              column(
                width = 10, offset = 1,
                style = "background-color: #e2e2e2; border:3px solid #8ebf42",
                shiny::sliderInput(
                  "trunc_length", label = "Max characters to show",
                  value = 20, min = 10, max = 30, step = 2
                )
              )
            )
          ),
          tags$hr(),
          htmltools::h4("Table Options"),
          fluidRow(
            column(
              width = 7,
              shiny::sliderInput(
                "ft_size", label = "Font Size",
                value = 9, min = 5, max = 12, step = 1
              )
            ),
            column(
              width = 5,
              tags$label("Subject Contrast"),
              shinyWidgets::switchInput(
                inputId = "subj_contrast",
                onLabel = "Heavy",
                offLabel = "Light",
                size = "small"
              )
            )
          ),
          fluidRow(
            column(
              width = 7,
              shiny::sliderInput(
                "digits", label = "Round Values",
                value = 3, min = 1, max = 6, step = 1,
                post = " digits"
              )
            )
          )
        ),
        maxWidth = "300px"
      )
    )
  )
}


#' Handle all global UIs
#'
#' @inheritParams v
#' @param input named list of shiny inputs
#' @param output named list of shiny outputs
#' @param session main shiny session
#'
#' @importFrom shiny reactive req observe observeEvent reactiveValues getDefaultReactiveDomain
#'
#' @returns a list of `reactiveValues`
#'
#' @keywords internal
v_global_server <- function(
    .df_list,
    input,
    output,
    session = getDefaultReactiveDomain()
){

  .rv <- reactiveValues()

  observe({
    # Selected Dataset
    .rv$data_select = .df_list[[shiny::req(input$data_select)]]
    # Global subject filter
    .rv$subject_filter = input$subject_filter
    # Frozen columns
    .rv$freeze_col_choices = input$freeze_cols

    # Arguments passed to create_v_datatable directly
    dt_args <- list(
      # Show Filters
      .show_filters = input$show_filters,
      # Label presence and formatting
      .show_labels = "show_labels" %in% input$dt_options,
      .wrap_labels = "wrap_labels" %in% input$dt_options,
      .trunc_labels = "trunc_labels" %in% input$dt_options,
      .trunc_length = input$trunc_length,
      # Misc DT options
      .digits = input$digits,
      .ft_size = input$ft_size,
      .subj_contrast = input$subj_contrast
    )
    .rv$dt_args <- dt_args

  })

  return(.rv)
}
