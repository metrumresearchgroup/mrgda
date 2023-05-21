#' Write an NMTRAN data set and corresponding meta data
#'
#' @description
#' This function will take a nonmem ready data frame in R and write it out to a nonmem ready csv.
#' It also creates a meta data folder, storing the xpt file along with other useful information.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .file csv file name to write out to (including path)
#' @param .prev_file csv file name of previous version (defaults to .file)
#' @param .compare_from_svn logical. Should the data comparison be done on the latest svn version? (If not, local version is used)
#' @param .return_base_compare logical. Should the two current and previous versions of the datasets be returned?
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' nm_write(.data = nm, .spec = nm_spec, .file = "data/derived/pk.csv")
#'}
#' @md
#' @export
nm_write <- function(.data, .spec, .file, .prev_file = NULL, .compare_from_svn = TRUE, .return_base_compare = FALSE) {

  if (tools::file_ext(.file) != "csv") {
    stop("'.file' must reference a 'csv' file")
  }

  .prev_file <- ifelse(is.null(.prev_file), .file, .prev_file)

  # Base Version for Diff ----------------------------------------
  base_df <- get_base_df(.prev_file, .compare_from_svn)$base_df

  # Write Out New Version ---------------------------------------------------
  data.table::fwrite(
    x = .data,
    file = .file,
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    na = "."
  )

  # cli::cli_alert_success(glue::glue("File written: {.file}"))

  # Prepare Meta Data Folder ------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)

  # Create directory anew if it exists
  if (dir.exists(.meta_data_folder)) {
    unlink(.meta_data_folder, recursive = TRUE)
    # cli::cli_alert_info(glue::glue("Directory removed: {.meta_data_folder}"))
  }

  dir.create(.meta_data_folder)
  # cli::cli_alert_success(glue::glue("Directory created: {.meta_data_folder}"))

  # Write Out Meta Data -----------------------------------------------------
  haven::write_xpt(
    data = yspec::ys_add_labels(.data, .spec),
    path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
    version = 5, # Use version 5
    name = paste0("a", substr(gsub("[^[:alnum:]]", "", .data_name), 1, 7)) # Max of 8 chars
  )

  # cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, paste0(.data_name, '.xpt'))}"))


  # Execute data diffs ------------------------------------------------------
  compare_df <- readr::read_csv(.file) %>% suppressMessages()

  if (!is.null(base_df)) {
    execute_data_diffs(base_df, compare_df, .meta_data_folder)
  }


  # Store system info -------------------------------------------------------
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

  # cli::cli_alert_success(glue::glue("File written: {file.path(.meta_data_folder, 'sys-info.yml')}"))


  # Determine and save dependencies -----------------------------------------
  dependencies <- find_in_files(.paths = c(here::here("script"), here::here("model")), .string = basename(.file))
  # yaml::write_yaml(dependencies, file = file.path(.meta_data_folder, "dependencies.yml"))



  # Return ------------------------------------------------------------------
  if (.return_base_compare) {
    return(
      list(
        base_df = base_df,
        compare_df = compare_df
      )
    )

  } else {
    return(NULL)
  }

}
