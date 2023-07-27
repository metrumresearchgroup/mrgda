#' Source Visualizer Shiny App
#'
#' This function creates a Shiny App for visualizing source data. It receives a list of data frames, each representing a different domain, and
#' produces a tabbed visualizer where each tab corresponds to a domain. A global filter is provided to filter the data by a subject column.
#'
#' @param .src_list A named list of data frames. Each data frame represents a domain that should be visualized. The list names should be the domain names.
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
src_viz <- function(.src_list, .subject_col = NULL) {

  if (!inherits(.src_list, "list")) {
    stop("The input must be a list of data frames.")
  }

  .box_title <- .src_list$mrgda_src_meta$path

  # Ensure list elements are named
  if(is.null(names(.src_list))){
    names(.src_list) <- paste("Dataframe", seq(length(.src_list)))
  }

  # Filter out mrgda specific dataframe
  .src_list <- .src_list[!grepl("mrgda", names(.src_list), fixed = TRUE)]

  # Determine .subject_col if not specified
  if(is.null(.subject_col)){
    .subject_col <- check_subject_col(.src_list)
  }else{
    # Make sure .subject_col is valid
    if(!any(purrr::map_lgl(.src_list, ~{.subject_col %in% names(.x)}))){
      abort(glue(".subject_col ({.subject_col}) is not present in any dataframe"))
    }
  }

  # Create global filter UI
  global_filter_ui <- create_global_filter(.subject_col)

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
              global_filter_ui
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

  server <- function(input, output, session) {

    purrr::map2(names(.src_list), .src_list, function(.domain, .df) {

      output[[paste0("table", .domain)]] <- DT::renderDataTable({

        has_subject_col <- !is.null(.subject_col) && .subject_col %in% names(.df)
        do_rows_filter <- has_subject_col && nchar(trimws(input$subject_filter)) > 0

        if(do_rows_filter){
          .df_filter <- .df %>% dplyr::filter(!!sym(.subject_col) == trimws(input$subject_filter))
        }else{
          .df_filter <- .df
        }

        # Allows for some dataframes to not have .subject_col
        if(has_subject_col){
          v(.df_filter, .subject_col)
        }else{
          v(.df, .subject_col = NULL)
        }


      })
    })

    session$onSessionEnded(shiny::stopApp)
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE)
  )
}


#' Search for ID columns across list of dataframes
#'
#' @inheritParams src_viz
#' @param id_cols vector of id columns to search for
#'
#' @keywords internal
check_subject_col <- function(.src_list, id_cols = c("ID", "USUBJID")){
  # Look for USUBJID and ID columns across datasets
  id_col_df <- purrr::map_dfr(.src_list, function(df){
    purrr::map_lgl(id_cols, function(id_col){
      any(grepl(glue("^(?i){id_col}$"), names(df)))
    }) %>% stats::setNames(id_cols)
  }) %>% dplyr::mutate(dataset = names(.src_list)) %>%
    tidyr::pivot_longer(all_of(id_cols), names_to = "id_col", values_to = "present")


  if(!any(id_col_df$present)){
    # No global filter if no id_cols are found
    subject_col <- NULL
  }else{
    # Use the id_col with the most occurrences across the datasets
    id_count <- id_col_df %>% dplyr::count(id_col, present)
    subject_col <- id_count$id_col[id_count$n == max(id_count$n)]
    # Use USUBJID if they are the same frequency
    if(length(subject_col) > 1){
      subject_col <- "USUBJID"
    }
  }

  return(subject_col)
}


#' Create global filter
#'
#' @inheritParams src_viz
#'
#' @keywords internal
create_global_filter <- function(.subject_col){
  if(!is.null(.subject_col)){
    global_filter_ui <-
      shiny::textInput(
        inputId = "subject_filter",
        label = paste0("Global ", .subject_col, " Filter"),
        width = "100%"
      )

  }else{
    global_filter_ui <- htmltools::div()
  }

  return(global_filter_ui)
}
