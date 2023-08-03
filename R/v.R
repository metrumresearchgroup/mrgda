#' View a formatted dataframe in the viewer
#'
#' Creates a DT::datatable object and runs it in a background shiny application in the Rstudio viewer pane.
#'
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
    .df,
    .subject_col = NULL,
    .freeze_cols = NULL,
    .digits = 3
){

  args <- list(
    .df = .df,
    .subject_col = .subject_col,
    .freeze_cols = .freeze_cols,
    .digits = .digits
  )

  if(object.size(.df) > 2.1e6){
    # Run in shiny app background process for large dataframes
    run_app_bg(v_shiny_internal, args = args)
  }else{
    # Create DT::datatable object on client side otherwise
    do.call(create_v_datatable, args = args)
  }
}



#' Basic shiny app for running `mrgda::create_v_datatable()` on the server
#'
#' @inheritParams create_v_datatable
#' @inheritParams run_app_bg
#'
#' @keywords internal
v_shiny_internal <- function(
    .df,
    .subject_col,
    .freeze_cols,
    .digits,
    host = NULL,
    port = NULL
){

  # For Development Environment
  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    message("Loading ", load_path)
    devtools::load_all(load_path)
  }

  ui <- shiny::fluidPage(
    v_ui("df_view")
  )

  server <- function(input, output, session) {
    v_server("df_view", .df, .subject_col, .freeze_cols, .digits)

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
