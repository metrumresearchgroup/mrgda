#' Explain the Reasoning for a Section of Code
#'
#' This function prints a formatted message explaining the reasoning for a section of code
#' and then prints the result of evaluating the provided expression. This function is
#' intended to be run interactively in the R console to aid in code understanding and debugging.
#'
#' @param .msg A character string containing the explanatory message for the code.
#' @param .expr An expression representing the R code to be executed.
#'
#' @return Invisible NULL. This function is used for its side effect of printing to the console.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- Theoph
#' dat$Time[2] <- 0
#' explain("Subject 1 has duplicate time 0", {
#'   dat %>% filter(Subject == 1, Time == 0)
#' })
#' }
#'
explain <- function(.msg, .expr) {

  cli::cli_h1(paste0("Explanation: ", .msg))

  message("")

  print(.expr)

  cli::cli_h1("")

  return(invisible(NULL))
}
