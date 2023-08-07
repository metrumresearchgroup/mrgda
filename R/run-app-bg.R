#' Run application in background for single user
#'
#' Use [callr::r_bg()] to launch the app in a separate process, returning once
#' `port` on `host` is active.
#'
#' @param func Function passed as the `func` argument to [callr::r_bg()]. This
#'   must meet the following requirements:
#'   * **launch** a Shiny app
#'   * accept `host` and `port` arguments and instruct Shiny to use those
#'   * call [devtools::load_all()] with the value of the
#'     `MRGDA_SHINY_DEV_LOAD_PATH` environment variable if it is a non-empty
#'     string.
#' @param args Argument passed as `args` argument to [callr::r_bg()]. This
#'   should not include "host" or "port"; those will be added by this function.
#'   Must be a named list.
#' @param host,port Host and port to serve the Shiny app on. If `port` isn't
#'   specified, it is randomly selected from the valid range of dynamic ports.
#'
#' @details
#' Make sure to set `Sys.setenv('MRGDA_SHINY_DEV_LOAD_PATH' = here::here())`
#' if you are in a development setting. This will allow package functions to be
#' accessible in the background process, which is not an issue when the package
#' is installed.
#'
#' @examples
#'
#' \dontrun{
#'
#' shiny_func <- function(
#'    df,
#'    filter = TRUE,
#'    host = NULL,
#'    port = NULL
#' ){
#'
#'  # For Development Environment, must load the package
#'  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
#'  if (nzchar(load_path)) {
#'    message("Loading ", load_path)
#'    devtools::load_all(load_path)
#'  }
#'
#'  # other setup...
#'
#'  ui <- fluidPage
#'  server <- function(input, output, session) {}
#'
#'  # Passes the host and port to the shinyApp object
#'  app <- shinyApp(ui = ui, server = server,
#'                  options = list(host = host, port = port))
#'
#'  # Function launches the app
#'  shiny::runApp(app)
#' }
#'
#' # Launching app in background
#' args <- list(df = mtcars, filter = TRUE)
#' run_app_bg(shiny_func, args = args)
#'
#' }
#'
#' @return The callr process (invisibly).
#' @keywords internal
run_app_bg <- function(func, args,
                       host = getOption("shiny.host", "127.0.0.1"),
                       port = getOption("shiny.port")) {
  port <- port %||% random_dynamic_port(host = host)
  args <- c(args, list(host = host, port = port))
  env <- callr::rcmd_safe_env()
  load_path <- Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    env <- c(env, "MRGDA_SHINY_DEV_LOAD_PATH" = load_path)
  }

  iodir <- tempfile(pattern = "mrgda--")
  fs::dir_create(iodir)
  stderr_file <- file.path(iodir, "stderr")

  # The following approach (callr::r_bg() followed by a pingr::is_up()
  # while-loop) is based on target's tar_watch() (MIT license).
  process <- callr::r_bg(
    func = func,
    args = args,
    env = env,
    stdout = file.path(iodir, "stdout"),
    stderr = stderr_file,
    supervise = TRUE,
    package = "mrgda")

  url <- paste0("http://", host, ":", port)
  spinner <- cli::make_spinner()
  while (!pingr::is_up(destination = host, port = port)) {
    if (!process$is_alive()) {
      msgs <- fmt_bg_file(stderr_file)

      cli::cli_abort(c(
        "x" = paste("Background app process at", url, "failed. Callback:\n"),
        "",
        msgs,
        "",
        "i" = "If you are in a dev environment, make sure you set the environment variable:",
        " " = "{.code Sys.setenv('MRGDA_SHINY_DEV_LOAD_PATH' = here::here())}."
      ))
    }
    Sys.sleep(0.01)
    spinner$spin()
  }

  # Browse in Rstudio viewer
  # utils::browseURL(url,  browser = .rs.invokeShinyPaneViewer)
  rstudioapi::viewer(url, height = "maximize")

  return(invisible(process))
}


#' Assign dynamic port
#'
#' @param host Host to serve the Shiny app on
#' @param n Number of ports to try before giving up
#'
#' @return A port that is available to listen on
#'
#' @keywords internal
random_dynamic_port <- function(host, n = 20) {
  # TODO: Respect /proc/sys/net/ipv4/ip_local_port_range
  lower <- 49152L - 1L
  upper <- 65535L

  httpuv::randomPort(
    min = lower, max = upper,
    host = host, n = n)
}


#' Format lines of `stderr_file` into `cli::cli_bullets`
#'
#'
#' @details
#' Purpose of this function is to format errors or warnings with use of `v()`.
#' Failure of the background shiny app has somewhat verbose messages, and this
#' helps to emphasize what actually went wrong.
#'
#' See `?cli::cli_bullets` for how to change formatting
#'
#' @param bg_file file path of background file, such as `stderr`
#'
#' @return character vector formatted to be used with `cli::cli_bullets`
#' @keywords internal
fmt_bg_file <- function(bg_file){

  bg_lines <- readr::read_file(bg_file)
  msgs <- strsplit(bg_lines, '\n')[[1]]

  # Set comments to bullets by default
  msgs <- msgs %>% stats::setNames(rep(">", length(msgs)))
  # Remove bullets for empty lines
  names(msgs)[grepl("^\\s*$",msgs)] <- " "

  # Format loading, warning, and error messages (case insensitive)
  # (allow whitespace or ':' to surround words)
  cli_patterns <- c("error" = "x","loading" = "v", "warning" = "!")
  names(cli_patterns) <- glue("^(?i)\\s*{names(cli_patterns)}[:]?(\\s|$)")

  for (i in seq_along(cli_patterns)) {
    names(msgs)[grepl(names(cli_patterns)[i], msgs)] <- unname(cli_patterns[i])
  }

  return(msgs)
}
