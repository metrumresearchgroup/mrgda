

colourise <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  if (has_colour()) {
    unclass(cli::make_ansi_style(testthat_style(as))(text))
  } else {
    text
  }
}

has_colour <- function() {
  isTRUE(getOption("testthat.use_colours", TRUE)) &&
    cli::num_ansi_colors() > 1
}

testthat_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "yellow",
    warning = "magenta",
    failure = "red",
    error = "red"
  )[[type]]
}

summary_line <- function(n_fail, n_pass, n_skip) {
  colourise_if <- function(text, colour, cond) {
    if (cond) colourise(text, colour) else text
  }

  # Ordered from most important to least important
  paste0(
    "[ ",
    colourise_if("FAIL", "failure", n_fail > 0), " ", n_fail, " | ",
    colourise_if("SKIP", "skip", n_skip > 0), " ", n_skip, " | ",
    colourise_if("PASS", "success", n_pass > 0), " ", n_pass,
    " ]"
  )
}

