#' @noRd
show_app_exit_hint <- function(app_name) {
  cli::cli_rule(left = paste("Launching", app_name))
  cli::cli_alert_info("{.strong How to close the app and free your R console:}")
  cli::cli_ul()
  cli::cli_li("EITHER: close the browser tab/window running {.emph {app_name}}")
  cli::cli_li(
    "OR: click the {.strong Stop} button (red stop sign) above the R Console in RStudio"
  )
  cli::cli_end()
  invisible()
}
