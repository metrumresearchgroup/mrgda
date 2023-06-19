#' src_viz
#'
#' This function provides a Shiny app for visualizing source data organized by different domains and subjects.
#'
#' @param .src_list A named list of data.frames where each data.frame represents a different domain of source data.
#' The names of the list items should correspond to the domain names. One of the list items should be named "mrgda_labels"
#' and contain column labels for the various domains.
#'
#' The Shiny app provided by this function consists of a dashboard with two main sections: "Domains" and "Subject Drilldown".
#' In the "Domains" section, users can select a domain from the provided list and view related summary information and data.
#' In the "Subject Drilldown" section, users can enter a subject ID and choose the domains they want to display. The selected
#' domains' data related to the entered subject ID will be shown.
#'
#' @return A Shiny app for visualizing the source data.
#' @examples
#' \dontrun{
#' read_src_dir(here::here("inst", "example-sdtm")) %>% src_viz()
#' }
#' @internal
src_viz <- function(.src_list){

  domain_names <- names(.src_list)[!grepl("mrgda", names(.src_list), fixed=TRUE)]

  ui <-
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(titleWidth = 200),
      shinydashboard::dashboardSidebar(
        width = 200,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Domains", tabName = "domains", icon = shiny::icon("table")),
          shinydashboard::menuItem("Subject Drilldown", tabName = "subjects", icon = shiny::icon("person"))
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "domains",
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shinydashboard::box(
                  title = "Domain Selection",
                  solidHeader = TRUE,
                  status = "primary",
                  width = NULL,
                  shiny::selectInput(
                    inputId = "domain",
                    label = "Domain",
                    choices = domain_names,
                    selectize = FALSE,
                    size = 10
                  )
                )
              ),
              shiny::column(
                width = 9,
                shinydashboard::box(
                  title = "Summary Information",
                  solidHeader = TRUE,
                  status = "primary",
                  width = NULL,
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shinydashboard::valueBoxOutput("nSubj", width = "100%")
                    ),
                    shiny::column(
                      width = 4,
                      DT::DTOutput("domainLabels")
                    ),
                    shiny::column(
                      width = 4,
                      DT::DTOutput("uniqueValues")
                    )
                  )

                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = "Data View",
                  solidHeader = TRUE,
                  status = "primary",
                  width = NULL,
                  DT::DTOutput("domainData")
                )
              )
            )
          ),

          shinydashboard::tabItem(tabName = "subjects",
                                  shiny::fluidRow(
                                    shiny::column(
                                      width = 3,
                                      shinydashboard::box(
                                        width = NULL,
                                        status = "primary",
                                        solidHeader = TRUE,
                                        title = "Subject ID",
                                        shiny::textInput("usubjid", "USUBJID", value = "")
                                      )
                                    ),
                                    shiny::column(
                                      width = 9,
                                      shinydashboard::box(
                                        width = NULL,
                                        title = "Domains to Display",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        shiny::checkboxGroupInput(
                                          "subjectDomains",
                                          "Domains to Display",
                                          choices = domain_names,
                                          selected = domain_names,
                                          inline = TRUE
                                        )
                                      )
                                    )
                                  ),
                                  shiny::uiOutput("subjectUI")
          )
        )
      )
    )

  server <- function(input, output) {

    rV <- shiny::reactiveValues()

    observeEvent(input$domain, {
      rV$domainData <- .src_list[[input$domain]]
      rV$domainLabels <-
        .src_list$mrgda_labels %>%
        dplyr::filter(DOMAIN == input$domain) %>%
        dplyr::select(COLUMN_NAME, COLUMN_LABEL)
    })

    output$domainData <- DT::renderDT({
      rV$domainData %>%
        DT::datatable(
          rownames = FALSE,
          filter = "top",
          class = 'compact cell-border stripe',
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
        )
    })


    output$domainLabels <- DT::renderDT(
      rV$domainLabels %>%
        DT::datatable(
          rownames = FALSE,
          selection = "single",
          filter = "top",
          class = 'compact cell-border stripe',
          options = list(
            pageLength = 10,
            sorting = FALSE
          )
        )
    )


    output$nSubj <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        subtitle = "N Subjects",
        value = length(unique(.src_list[[input$domain]]$USUBJID)),
        icon = shiny::icon("people-group"),
      )
    })

    output$uniqueValues <- DT::renderDT({
      req(input$domainLabels_rows_selected)
      table(rV$domainData[[rV$domainLabels$COLUMN_NAME[input$domainLabels_rows_selected]]]) %>%
        as.data.frame() %>%
        dplyr::rename(Value = Var1, N = Freq) %>%
        dplyr::arrange(-N) %>%
        DT::datatable(
          rownames = FALSE,
          selection = "none",

          class = 'compact cell-border stripe',
          options = list(
            pageLength = 10,
            sorting = FALSE,
            searching = FALSE
          )
        )
    })

    observeEvent(list(input$usubjid, input$subjectDomains), {

      req(input$usubjid)

      rV$subject_ui <- tagList()

      for(i in domain_names){

        if(!(i %in% input$subjectDomains)){
          next
        }

        rV$subject_ui <- shiny::tagAppendChild(
          rV$subject_ui,
          shinydashboard::box(
            width = 12,
            title = i,
            solidHeader = TRUE,
            status = "primary",
            try(
              .src_list[[i]] %>%
                dplyr::filter(USUBJID == input$usubjid) %>%
                DT::datatable(
                  rownames = FALSE,
                  filter = "top",
                  class = 'compact cell-border stripe',
                  options = list(
                    pageLength = 5,
                    scrollX = TRUE
                  ),
                )
            )
          )
        )


      }

    })

    output$subjectUI <- shiny::renderUI({
      rV$subject_ui
    })


  }

  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}
