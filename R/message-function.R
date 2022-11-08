message_function <- function(.function_name){

  cli::cat_boxx(
    header = paste0(.function_name, "() formula:"),
    cli::code_highlight(deparse(get(.function_name)))
  )

}
