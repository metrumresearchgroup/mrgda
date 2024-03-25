#' Silence All Console Outputs from an Expression
#'
#' Executes an expression while silencing all messages, warnings, errors, and console outputs.
#' This includes suppressing messages, warnings, catching errors without printing them to the console,
#' and suppressing all printed output.
#'
#' @param .expr An expression to be evaluated.
#'
#' @return The result of the expression `.expr` if it completes successfully.
#'
#' @noRd
silence_console_output <- function(.expr) {
  # Start diverting all output to a temporary file
  temp_file <- tempfile()
  sink(temp_file)
  on.exit({
    sink()
    unlink(temp_file)  # Clean up by removing the temporary file
  })

  try({
    suppressWarnings(suppressMessages(.expr))
  })
}
