#' Load the mrgda Shiny app
#'
#' Launch an interactive Shiny app for exploring a CSV data set. The Visualizer tab
#' provides a scatter plot with selectable X/Y axes, optional color grouping, hover
#' fields (up to five), and ad hoc filters for numeric ranges or categorical values.
#' The Tabulizer tab renders a searchable table view of the same data.
#'
#' CSV files are read with [read_csv_dots()], so "." values are interpreted as `NA`.
#' If a YAML specification is supplied, it is loaded with [yspec::ys_load()] and
#' applied via [yspec::ys_factors()] to coerce labeled factors before rendering.
#'
#' @param .csv_path Path to a CSV data file.
#' @param .spec_path Optional path to a YAML specification file for factor metadata.
#' @return A Shiny app object.
#' @export
#'
#' @examples
#' \dontrun{
#' visualize_data("analysis/adsl.csv")
#' visualize_data("analysis/adsl.csv", "analysis/adsl.yaml")
#' }
visualize_data <- function(.csv_path, .spec_path = NULL) {
  if (!file.exists(.csv_path)) {
    stop("`.csv_path` does not exist: ", .csv_path, call. = FALSE)
  }

  data <- read_csv_dots(.csv_path)

  # Apply factors to data
  if (!is.null(.spec_path)) {

    # Confirm spec path exists
    if (!file.exists(.spec_path)) {
      stop("`.spec_path` does not exist: ", .spec_path, call. = FALSE)
    }

    # Load in spec
    spec <- yspec::ys_load(.spec_path)

    data <- yspec::ys_factors(data, spec)
  }

  data_vars <- names(data)
  default_x <- if (length(data_vars) > 0) data_vars[[1]] else ""
  default_y <- if (length(data_vars) > 1) data_vars[[2]] else default_x
  default_hover <- head(data_vars, 5)

  ui <- bslib::page_navbar(
    title = NULL,
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel(
      "Visualizer",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            inputId = "x_var",
            label = "X-axis",
            choices = data_vars,
            selected = default_x
          ),
          shiny::selectInput(
            inputId = "y_var",
            label = "Y-axis",
            choices = data_vars,
            selected = default_y
          ),
          shiny::selectInput(
            inputId = "color_var",
            label = "Color by",
            choices = c("None" = "", data_vars),
            selected = ""
          ),
          shiny::selectizeInput(
            inputId = "filter_vars",
            label = "Filter variables",
            choices = data_vars,
            multiple = TRUE
          ),
          shiny::uiOutput("filter_ui"),
          shiny::selectizeInput(
            inputId = "hover_vars",
            label = "Hover variables (max 5)",
            choices = data_vars,
            selected = default_hover,
            multiple = TRUE,
            options = list(maxItems = 5)
          )
        ),
        plotly::plotlyOutput("scatter_plot")
      )
    ),
    bslib::nav_panel(
      "Tabulizer",
      DT::dataTableOutput("tabulizer_data")
    ),
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() shiny::stopApp())

    # show hint once the server has started
    show_app_exit_hint("diffDashboard")

    filter_map <- shiny::reactive({
      vars <- input$filter_vars
      if (is.null(vars) || length(vars) == 0) {
        return(NULL)
      }
      ids <- make.names(vars, unique = TRUE)
      stats::setNames(ids, vars)
    })

    output$filter_ui <- shiny::renderUI({
      vars <- input$filter_vars
      if (is.null(vars) || length(vars) == 0) {
        return(NULL)
      }
      mapping <- filter_map()
      ui_list <- lapply(vars, function(var) {
        id_base <- mapping[[var]]
        filter_data <- data[[var]]
        if (is.numeric(filter_data)) {
          rng <- range(filter_data, na.rm = TRUE)
          if (!all(is.finite(rng))) {
            return(NULL)
          }
          shiny::sliderInput(
            inputId = paste0("filter_num__", id_base),
            label = paste0(var, " range"),
            min = rng[1],
            max = rng[2],
            value = rng
          )
        } else {
          choices <- sort(unique(as.character(filter_data)))
          shiny::selectizeInput(
            inputId = paste0("filter_cat__", id_base),
            label = paste0(var, " values"),
            choices = choices,
            selected = choices,
            multiple = TRUE
          )
        }
      })
      do.call(shiny::tagList, ui_list)
    })

    output$tabulizer_data <- DT::renderDataTable({
      DT::datatable(
        data,
        filter = "top",
        options = list(pageLength = 25, autoWidth = TRUE)
      )
    })

    filtered_data <- shiny::reactive({
      vars <- input$filter_vars
      if (is.null(vars) || length(vars) == 0) {
        return(data)
      }
      mapping <- filter_map()
      plot_data <- data
      for (var in vars) {
        id_base <- mapping[[var]]
        filter_data <- plot_data[[var]]
        if (is.numeric(filter_data)) {
          range_id <- paste0("filter_num__", id_base)
          rng <- input[[range_id]]
          if (is.null(rng) || length(rng) != 2) {
            next
          }
          keep <- filter_data >= rng[1] & filter_data <= rng[2]
          plot_data <- plot_data[keep, , drop = FALSE]
        } else {
          cat_id <- paste0("filter_cat__", id_base)
          vals <- input[[cat_id]]
          if (is.null(vals) || length(vals) == 0) {
            plot_data <- plot_data[FALSE, , drop = FALSE]
            next
          }
          keep <- as.character(filter_data) %in% vals
          plot_data <- plot_data[keep, , drop = FALSE]
        }
      }
      plot_data
    })

    output$scatter_plot <- plotly::renderPlotly({
      plot_data <- filtered_data()
      shiny::req(input$x_var, input$y_var)
      x <- plot_data[[input$x_var]]
      y <- plot_data[[input$y_var]]
      hover_vars <- input$hover_vars
      hover_vars <- hover_vars[hover_vars %in% data_vars]
      hover_vars <- head(hover_vars, 5)
      hover_text <- NULL
      if (length(hover_vars) > 0) {
        hover_df <- plot_data[, hover_vars, drop = FALSE]
        hover_df[] <- lapply(hover_df, function(col) {
          if (is.factor(col)) {
            as.character(col)
          } else {
            col
          }
        })
        hover_text <- apply(
          hover_df,
          1,
          function(row) paste(paste0(hover_vars, ": ", row), collapse = "<br>")
        )
      }
      if (!is.null(input$color_var) && nzchar(input$color_var)) {
        color_data <- plot_data[[input$color_var]]
        plotly::plot_ly(
          x = x,
          y = y,
          type = "scatter",
          mode = "markers",
          color = color_data,
          text = hover_text,
          hoverinfo = if (is.null(hover_text)) "x+y" else "text"
        )
      } else {
        plotly::plot_ly(
          x = x,
          y = y,
          type = "scatter",
          mode = "markers",
          text = hover_text,
          hoverinfo = if (is.null(hover_text)) "x+y" else "text"
        )
      }
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(.csv_path = .csv_path, .spec_path = .spec_path,
                   launch.browser = TRUE, quiet = TRUE)
  )
}
