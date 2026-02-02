#' Load the mrgda Shiny app
#'
#' Launch an interactive Shiny app for exploring a CSV data set. The Visualizer view
#' provides a scatter plot with selectable X/Y axes, optional color grouping, hover
#' fields based on the active axis/color/facet/filter selections, and ad hoc filters
#' for numeric ranges or categorical values.
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

    data <- yspec::ys_add_factors(data, spec, .suffix = "")
  }

  data_vars <- names(data)
  default_x <- if (length(data_vars) > 0) data_vars[[1]] else ""
  default_y <- if (length(data_vars) > 1) data_vars[[2]] else default_x
  default_hover <- unique(c(default_x, default_y))

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
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
        shiny::selectInput(
          inputId = "facet_var",
          label = "Facet by",
          choices = c("None" = "", data_vars),
          selected = ""
        ),
        shiny::selectizeInput(
          inputId = "hover_vars",
          label = "Hover variables",
          choices = data_vars,
          selected = default_hover,
          multiple = TRUE
        ),
        shiny::selectizeInput(
          inputId = "filter_vars",
          label = "Filter variables",
          choices = data_vars,
          multiple = TRUE
        ),
        shiny::uiOutput("filter_ui")
      ),
      shiny::uiOutput("scatter_plot_ui")
    )
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

    auto_hover_vars <- shiny::reactive({
      vars <- c(
        input$x_var,
        input$y_var,
        if (!is.null(input$color_var) && nzchar(input$color_var)) input$color_var else NULL,
        if (!is.null(input$facet_var) && nzchar(input$facet_var)) input$facet_var else NULL,
        input$filter_vars
      )
      vars <- vars[!is.na(vars) & nzchar(vars)]
      unique(vars)
    })

    shiny::observeEvent(
      list(
        input$x_var,
        input$y_var,
        input$color_var,
        input$facet_var,
        input$filter_vars
      ),
      {
        selected <- unique(c(auto_hover_vars(), input$hover_vars))
        shiny::updateSelectizeInput(
          session,
          "hover_vars",
          choices = data_vars,
          selected = selected
        )
      },
      ignoreInit = FALSE
    )

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

    plot_spec <- shiny::reactive({
      plot_data <- filtered_data()
      shiny::req(input$x_var, input$y_var)
      x <- plot_data[[input$x_var]]
      y <- plot_data[[input$y_var]]
      keep_xy <- !is.na(x) & !is.na(y)
      plot_data <- plot_data[keep_xy, , drop = FALSE]
      x <- x[keep_xy]
      y <- y[keep_xy]
      if (nrow(plot_data) == 0) {
        return(list(empty = TRUE))
      }
      hover_vars <- unique(c(auto_hover_vars(), input$hover_vars))
      hover_vars <- hover_vars[hover_vars %in% data_vars]
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

      color_data <- NULL
      if (!is.null(input$color_var) && nzchar(input$color_var)) {
        color_data <- plot_data[[input$color_var]]
      }

      facet_data <- NULL
      if (!is.null(input$facet_var) && nzchar(input$facet_var)) {
        facet_data <- plot_data[[input$facet_var]]
      }

      base_df <- data.frame(
        x = x,
        y = y,
        hover_text = if (is.null(hover_text)) NA_character_ else hover_text,
        stringsAsFactors = FALSE
      )
      if (!is.null(color_data)) {
        base_df$color <- if (is.factor(color_data)) as.character(color_data) else color_data
      }
      if (!is.null(facet_data)) {
        facet_vals <- if (is.factor(facet_data)) as.character(facet_data) else facet_data
        facet_vals[is.na(facet_vals)] <- "(Missing)"
        base_df$facet <- facet_vals
      }

      list(
        empty = FALSE,
        base_df = base_df,
        has_color = !is.null(color_data),
        has_facet = !is.null(facet_data),
        tooltip = if (is.null(hover_text)) "x+y" else "text",
        x_label = input$x_var,
        y_label = input$y_var,
        color_label = if (!is.null(color_data)) input$color_var else NULL
      )
    })

    build_plot <- function(plot_df, spec, facets_per_plot = NULL) {
      gg_args <- list(
        data = plot_df,
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$y,
          text = .data$hover_text
        )
      )
      if (isTRUE(spec$has_color)) {
        gg_args$mapping <- ggplot2::aes(
          x = .data$x,
          y = .data$y,
          text = .data$hover_text,
          color = .data$color
        )
      }

      gg_plot <- do.call(ggplot2::ggplot, gg_args) +
        ggplot2::geom_point() +
        ggplot2::labs(
          x = spec$x_label,
          y = spec$y_label,
          color = spec$color_label
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(color = "grey85"),
          panel.background = ggplot2::element_rect(fill = "white"),
          plot.background = ggplot2::element_rect(fill = "white")
        )

      if (isTRUE(spec$has_facet)) {
        gg_plot <- gg_plot + ggplot2::facet_wrap(~facet, ncol = 3)
      }

      plotly::ggplotly(gg_plot, tooltip = spec$tooltip)
    }

    output$scatter_plot_ui <- shiny::renderUI({
      spec <- plot_spec()
      if (isTRUE(spec$empty)) {
        return(
          shiny::div(
            class = "text-muted",
            "No rows remain after filtering for non-missing X/Y values."
          )
        )
      }
      if (!isTRUE(spec$has_facet)) {
        output$scatter_plot_1 <- plotly::renderPlotly({
          build_plot(spec$base_df, spec)
        })
        return(plotly::plotlyOutput("scatter_plot_1", height = "800px"))
      }

      facet_levels <- unique(spec$base_df$facet)
      facet_groups <- split(
        facet_levels,
        ceiling(seq_along(facet_levels) / 9)
      )

      ui_list <- lapply(seq_along(facet_groups), function(i) {
        output_id <- paste0("scatter_plot_", i)
        facet_subset <- facet_groups[[i]]
        output[[output_id]] <- plotly::renderPlotly({
          plot_df <- spec$base_df[spec$base_df$facet %in% facet_subset, , drop = FALSE]
          build_plot(plot_df, spec)
        })
        plotly::plotlyOutput(output_id, height = "800px")
      })

      if (length(ui_list) > 1) {
        tabs <- lapply(seq_along(ui_list), function(i) {
          shiny::tabPanel(
            title = paste0("Facets ", i),
            ui_list[[i]]
          )
        })
        do.call(shiny::tabsetPanel, tabs)
      } else {
        do.call(shiny::tagList, ui_list)
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
