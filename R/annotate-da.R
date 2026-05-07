#' Annotate the Reasoning for a Section of Code
#'
#' This function prints a formatted message explaining the reasoning for a section of code
#' and then prints the result of evaluating the provided expression. This function is
#' intended to be run interactively in the R console to aid in code understanding and debugging.
#'
#' If the `mrgda.annotate_da_file` option is set to a file path, the explanation
#' and printed result are also appended to that file as Markdown.
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
#' annotate_da("Subject 1 has duplicate time 0", {
#'   dat %>% filter(Subject == 1, Time == 0)
#' })
#'
#' use_annotate_da_file("script/data-assembly/study-101-annotations.md")
#' annotate_da("Subject 1 has duplicate time 0", {
#'   dat %>% filter(Subject == 1, Time == 0)
#' })
#' }
#'
annotate_da <- function(.msg, .expr) {

  .old_tibble_options <- options(
    tibble.print_max = Inf,
    tibble.print_min = Inf,
    tibble.width = Inf
  )
  on.exit(options(.old_tibble_options), add = TRUE)

  cli::cli_h1(paste0("Explanation: ", .msg))

  message("")

  output <- utils::capture.output({
    result <- .expr
    print(result)
  })
  writeLines(output)

  .file <- getOption("mrgda.annotate_da_file", NULL)
  if (!is.null(.file)) {
    validate_annotate_da_file(.file)

    .annotation <- c(
      paste0("# Explanation: ", .msg),
      "",
      "```",
      output,
      "```",
      ""
    )

    .connection <- file(.file, open = "a")
    on.exit(close(.connection), add = TRUE)
    writeLines(.annotation, con = .connection)
  }

  cli::cli_h1("")

  return(invisible(NULL))
}

#' Set the Output File for `annotate_da()`
#'
#' Use this helper near the top of a data assembly script to configure where
#' subsequent [annotate_da()] calls should save their Markdown output. By default,
#' the target file is cleared so rerunning the same script starts a fresh annotation log.
#'
#' @param .file A single file path where `annotate_da()` output should be saved.
#'   The parent directory must already exist.
#' @param overwrite Should an existing file be cleared before annotation output is
#'   appended? Defaults to `TRUE`.
#'
#' @return The configured file path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' use_annotate_da_file("script/data-assembly/study-101-annotations.md")
#'
#' annotate_da("ID = 1 has outlier, will remove", {
#'   pk_data |> dplyr::filter(ID == 1)
#' })
#' }
#'
use_annotate_da_file <- function(.file, overwrite = TRUE) {
  validate_annotate_da_file(.file)

  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be a single {.code TRUE} or {.code FALSE}.")
  }

  if (overwrite && fs::file_exists(.file)) {
    fs::file_delete(.file)
  }

  if (overwrite) {
    fs::file_create(.file)
  }

  options(mrgda.annotate_da_file = .file)

  return(invisible(.file))
}

validate_annotate_da_file <- function(.file) {
  if (!is.character(.file) || length(.file) != 1 || is.na(.file) || !nzchar(.file)) {
    cli::cli_abort("Annotation output file must be a single non-empty file path.")
  }

  .dir <- fs::path_dir(.file)
  if (!fs::dir_exists(.dir)) {
    cli::cli_abort("Annotation output directory does not exist: {.path {.dir}}")
  }

  return(invisible(.file))
}
