#' Write an NMTRAN data set and corresponding meta data
#'
#' @description
#' This function will take a nonmem ready data frame in R and write it out to a nonmem ready csv.
#' It also creates a meta data folder, storing the xpt file along with other useful information.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .file csv file name to write out to (including path)
#' @param .script_path file path of data assembly script
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' nm_write(.data = nm, .spec = nm_spec, .file = "data/derived/pk.csv", .script_path = "script/data-assembly.Rmd")
#'}
#' @md
#' @export
nm_write <- function(.data, .spec, .file, .script_path) {

  if (tools::file_ext(.file) != "csv") {
    stop("'.file' must reference a 'csv' file")
  }

  # Read in Current Version for Diff ----------------------------------------
  if (file.exists(.file)) {
    .current_nm <- readr::read_csv(.file) %>% suppressMessages()
  } else {
    .current_nm <- NULL
  }


  # Write Out New Version ---------------------------------------------------
  data.table::fwrite(
    x = .data,
    file = .file,
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    na = "."
  )

  cli::cli_alert_success(glue::glue("File written: {.file}"))

  # Prepare Meta Data Folder ------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)

  # Create directory anew if it exists
  if (dir.exists(.meta_data_folder)) {
    unlink(.meta_data_folder, recursive = TRUE)
    cli::cli_alert_info(glue::glue("Directory removed: {.meta_data_folder}"))
  }

  dir.create(.meta_data_folder)
  cli::cli_alert_success(glue::glue("Directory created: {.meta_data_folder}"))

  # Write Out Meta Data -----------------------------------------------------
  haven::write_xpt(
    data = yspec::ys_add_labels(.data, .spec),
    path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
    version = 5, # Use version 5
    name = substr(gsub("[^[:alnum:]]", "", .data_name), 1, 8) # Max of 8 chars
  )

  cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, paste0(.data_name, '.xpt'))}"))


  yspec::ys_document(
    x = .spec,
    output_dir = .meta_data_folder,
    quiet = TRUE,
    stem = "define"
  ) %>% suppressWarnings()

  cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, 'define.pdf')}"))


  if (!is.null(.current_nm)) {
    diffdf::diffdf(
      base = .current_nm,
      compare = readr::read_csv(.file) %>% suppressMessages(),
      file = file.path(.meta_data_folder, "data-diff.txt"),
      suppress_warnings = TRUE
    )
  }

  cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, 'data-diff.txt')}"))


  .sys_info <- Sys.info()
  .r_version <- R.Version()
  .sys_time <- Sys.time()

  .sys_print <- list(
    User = .sys_info[['user']],
    Datetime = as.character(.sys_time),
    `R Version` = .r_version$version.string,
    Release = .sys_info[['release']],
    Version = .sys_info[['version']]
  )

  yaml::write_yaml(.sys_print, file = file.path(.meta_data_folder, "sys-info.yml"))

  cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, 'sys-info.yml')}"))

  # Data assembly script
  if (file.exists(.script_path)) {
    .script_path_absolute <- tools::file_path_as_absolute(.script_path)
    .extension <- paste0("data-assembly-script.", tools::file_ext(.script_path))
    file.copy(.script_path_absolute, file.path(.meta_data_folder, .extension), overwrite = TRUE)
    cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, .extension)}"))
  } else {
    cli::cli_alert_danger("Invalid .script_path provided")
    stop()
  }


  # Write script dependencies
  dependencies <- find_in_files(.paths = c(here::here("script"), here::here("model")), .string = basename(.file))
  yaml::write_yaml(dependencies, file = file.path(.meta_data_folder, "dependencies.yml"))

}
