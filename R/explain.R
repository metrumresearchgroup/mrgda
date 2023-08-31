#' Run and Explain Code Expressions
#'
#' This function runs one or more code expressions and provides an explanation along with the result.
#' It is intended for interactive use to better understand what each code chunk is doing.
#'
#' @param message A character string explaining what the code chunks are intended to do.
#' @param ... One or more code expressions to be run.
#' 
#' @return This function prints to the console and does not return anything.
#' 
#' @examples
#' explain("This code sums 1, 2, and 3, and calculates the mean of 1 to 5.",
#'                 sum(1, 2, 3),
#'                 mean(c(1, 2, 3, 4, 5)))
explain <- function(message, ...) {
  cli::cli_h1("Explanation")
  cli::cli_text(message)
  cli::cli_text(strrep("-", 40))
  
  # Capture the expressions
  exprs <- as.list(substitute(list(...)))[-1L]
  
  for(i in seq_along(exprs)) {
    cli::cli_h2(glue::glue("Result of Expression {i}"))
    result <- eval(exprs[[i]])
    cli::cli_text("Result:")
    print(result)
    cli::cli_text(strrep("-", 40))
  }
}
