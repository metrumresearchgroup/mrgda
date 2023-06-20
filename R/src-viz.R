#' src_viz function
#'
#' This function launches a Shiny app that allows a user to visualize a list of data frames.
#' The user can select a data frame from the list to view, filter it based on the 'USUBJID' column,
#' and color the rows alternately based on the values in the 'USUBJID' column.
#' The 'label' attribute of each column, if present, is displayed under the column name.
#'
#' @param .src_list A named list of data frames to be visualized.
#'
#' @return None. The function launches a Shiny app.
#' @export
#'
#' @examples
#' read_src_dir(here::here("inst", "example-sdtm")) %>% src_viz()
src_viz <- function(.src_list) {
  # Error handling for input
  if (!is.list(.src_list)) {
    stop("The input must be a list of data frames.")
  }

  if (!all(sapply(.src_list, is.data.frame))) {
    stop("All elements of the list must be data frames.")
  }

  .src_list <- .src_list[!grepl("mrgda", names(.src_list), fixed = TRUE)]

  # Define color by columns
  color_cols <- c("USUBJID")

  # Define the UI
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Source Visualizer"),
    shinydashboard::dashboardSidebar(
      shiny::selectInput("select_df", "Domain(s) View:",
                         choices = names(.src_list),
                         multiple=TRUE,
                         selected = names(.src_list)[1]),
      shiny::textInput("usubjid_filter", "USUBJID Filter:"),
      shiny::selectInput("color_by", "Split By:", choices = color_cols)
    ),
    shinydashboard::dashboardBody(shiny::uiOutput("dfDisplay"))
  )

  # Define server logic
  server <- function(input, output) {
    # Shiny module UI
    dfSelectionUI <- function(id) {
      ns <- shiny::NS(id)
      shinydashboard::box(
        solidHeader = TRUE,
        status = "primary",
        DT::DTOutput(ns("table")),
        title = id,
        collapsible = TRUE,
        width = 12
      )
    }

    # Shiny module server
    dfSelectionServer <-
      function(id, df, usubjid_filter, color_by) {
        shiny::moduleServer(id, function(input, output, session) {
          ns <- session$ns

          output$table <- DT::renderDT({
            tryCatch({
              data <- df()

              # Filter data
              if (usubjid_filter() != "" && "USUBJID" %in% names(data)) {
                data <- dplyr::filter(data, USUBJID == usubjid_filter())
              }

              # If 'color_by' column is selected and exists in the data frame, color the rows
              if (!is.null(color_by()) && color_by() %in% names(data)) {

                colorIndex <- which(names(data) == color_by()) - 1

                tableOpts = list(
                  scrollX = TRUE,
                  searchHighlight = TRUE,
                  rowGroup = list(dataSrc = colorIndex)
                )
              } else {
                tableOpts = list(scrollX = TRUE, rowGroup = list(dataSrc = colorIndex))
              }

              # Retrieve labels and modify column names
              labels <-
                sapply(colnames(data), function(col)
                  attr(data[[col]], "label"))
              names_with_labels <-
                paste("<b>",
                      colnames(data),
                      "</b><br>",
                      "<i>",
                      labels,
                      "</i>",
                      sep = "")
              names(data) <- names_with_labels

              # Return the data table
              DT::datatable(
                data,
                rownames = FALSE,
                escape = FALSE,
                filter = "top",
                selection = "single",
                class = 'compact cell-border',
                extensions = 'RowGroup',
                options = tableOpts
              )

            }, error = function(e) {
              shiny::showNotification(
                paste(
                  "Error: An error occurred while processing the data frame.",
                  e$message
                ),
                type = "error"
              )
              stop(e)
            })
          })
        })
      }

    output$dfDisplay <- shiny::renderUI({
      df_names <- input$select_df
      shiny::tagList(lapply(df_names, dfSelectionUI))
    })

    observe({
      df_names <- input$select_df
      lapply(df_names, function(df_name) {
        dfSelectionServer(
          df_name,
          reactive(.src_list[[df_name]]),
          reactive(input$usubjid_filter),
          reactive(input$color_by)
        )
      })
    })
  }

  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser=TRUE))
}
