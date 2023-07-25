#' Source Visualizer Shiny App
#'
#' This function creates a Shiny App for visualizing source data. It receives a list of data frames, each representing a different domain, and
#' produces a tabbed visualizer where each tab corresponds to a domain. A global filter is provided to filter the data by a subject column.
#'
#' @param .src_list A list of data frames. Each data frame represents a domain that should be visualized. The list names should be the domain names.
#' @param .subject_col A string specifying the name of the subject column for filtering. Defaults to "USUBJID".
#'
#' @return A Shiny App for visualizing source data from various domains.
#' @note The Shiny App will be launched in the system's default web browser.
#' @note This function assumes the use of the shinydashboard, shiny and purrr packages, and they should be installed and loaded.
#'
#' @usage
#' src_viz(.src_list, .subject_col = "USUBJID")
#'
#' @examples
#' \dontrun{
#' src_viz(read_src_dir(system.file("example-sdtm", package = "mrgda")), .subject_col = "USUBJID")
#' }
#'
#' @export
src_viz <- function(.src_list, .subject_col = "USUBJID") {

  if (!inherits(.src_list, "list")) {
    stop("The input must be a list of data frames.")
  }

  .box_title <- .src_list$mrgda_src_meta$path

  .src_list <- .src_list[!grepl("mrgda", names(.src_list), fixed = TRUE)]

  ui <-
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Source Visualizer"),
      shinydashboard::dashboardSidebar(disable = TRUE),
      shinydashboard::dashboardBody(
        shinydashboard::box(
          width = 12,
          title = .box_title,
          status = "primary",
          solidHeader = TRUE,
          shiny::fluidRow(
            shiny::column(
              width = 3,
              offset = 9,
              shiny::textInput(
                inputId = "subject_filter",
                label = paste0("Global ", .subject_col, " Filter"),
                width = "100%"
              )
            )
          ),
          # Make a tab for every domain
          do.call(
            shiny::tabsetPanel,
            purrr::map(names(.src_list), ~ {
              shiny::tabPanel(
                title = .x,
                shiny::tags$br(),
                DT::dataTableOutput(paste0("table", .x))
              )
            }
            )
          )
        )
      )
    )

  server <- function(input, output) {
    purrr::map2(names(.src_list), .src_list, function(.domain, .df) {
      output[[paste0("table", .domain)]] <- DT::renderDataTable({

        do_rows_filter <-
          (.subject_col %in% names(.df)) &
          nchar(trimws(input$subject_filter)) > 0

        if(do_rows_filter){

          rows_keep <-
            trimws(as.character(.df[[.subject_col]])) == trimws(input$subject_filter)

          .df <- .df[rows_keep, ]
        }

        view_src(.df, .subject_col, .view = "viewer")
      })
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE)
  )
}
