#' Run application in background for single user
#'
#' Use [callr::r_bg()] to launch the app in a separate process, returning once
#' `port` on `host` is active.
#'
#' @param func Function passed as the `func` argument to [callr::r_bg()]. This
#'   must meet the following requirements:
#'   * launch a Shiny app
#'   * accept `host` and `port` arguments and instruct Shiny to use those
#'   * call [devtools::load_all()] with the value of the
#'     MRGDA_SHINY_DEV_LOAD_PATH environment variable if it is a non-empty
#'     string.
#' @param args Argument passed as `args` argument to [callr::r_bg()]. This
#'   should not include "host" or "port"; those will be added by this function.
#'   Must be a named list.
#' @param host,port Host and port to serve the Shiny app on. If `port` isn't
#'   specified, it is randomly selected from the valid range of dynamic ports.
#' @return The callr process (invisibly).
#' @keywords internal
run_app_bg <- function(func, args,
                       host = getOption("shiny.host", "127.0.0.1"),
                       port = getOption("shiny.port")) {
  port <- port %||% random_dynamic_port()
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
      msgs <- fmt_bg_msgs(stderr_file)

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
  utils::browseURL(url,  browser = .rs.invokeShinyPaneViewer)

  return(invisible(process))
}

random_dynamic_port <- function() {
  # TODO: Respect /proc/sys/net/ipv4/ip_local_port_range
  #
  # TODO: Check if port is already bound.
  lower <- 49152L - 1L
  upper <- 65355L
  lower + sample(upper - lower, size = 1L)
}


#' Format lines of `stderr_file` into `cli::cli_bullets`
#'
#' @param stderr_file file path the standard error of the child R process is written to.
#'
#' @keywords internal
fmt_bg_msgs <- function(stderr_file){
  msgs <- strsplit(readr::read_file(stderr_file), '\n')[[1]]
  msgs <- msgs %>% stats::setNames(rep(">", length(msgs)))
  names(msgs)[grepl("(?i)Error", msgs)] <- "x"
  names(msgs)[grepl("(?i)Loading", msgs)] <- "v"
  names(msgs)[grepl("(?i)Warning", msgs)] <- "!"

  return(msgs)
}
