#' Write a derived data set and corresponding metadata
#'
#' @description
#' This function will take a data frame in R and write it out to csv.
#' It also creates a metadata folder, storing the xpt file along with other useful information.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .file csv file name to write out to (including path)
#' @param .subject_col subject column name (defaults to ID)
#' @param .prev_file csv file name of previous version (defaults to .file)
#' @param .compare_from_svn logical. Should the data comparison be done on the latest svn version? (If not, local version is used)
#' @param .return_base_compare logical. Should the two current and previous versions of the datasets be returned?
#' @param .execute_diffs logical. Should the diffs be executed?
#' @examples
#'\dontrun{
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#' write_derived(.data = nm, .spec = nm_spec, .file = "data/derived/pk.csv")
#'}
#' @md
#' @export
write_derived <- function(.data, .spec, .file, .subject_col = "ID", .prev_file = NULL, .compare_from_svn = TRUE, .return_base_compare = FALSE, .execute_diffs = TRUE) {

  if (tools::file_ext(.file) != "csv") {
    stop("'.file' must reference a 'csv' file")
  }

  spec_check <- yspec::ys_check(.data, .spec, error_on_fail = FALSE) %>% suppressMessages()

  if (!spec_check) {
    stop("Spec check failed. Please run 'yspec::ys_check()' and ensure it passes.", call. = FALSE)
  }

  .prev_file <- ifelse(is.null(.prev_file), .file, .prev_file)

  # Base Version for Diff ----------------------------------------
  base_df_list <- get_base_df(.prev_file, .compare_from_svn)

  # Check for commas in data
  check_for_commas <- purrr::map(.data, ~ any(stringr::str_detect(string = .x, pattern = ","), na.rm = TRUE))

  check_for_commas <- check_for_commas[unlist(check_for_commas)]
  if (length(check_for_commas) > 0) {
    cli::cli_abort(paste0("Comma found in following column(s):", paste(names(check_for_commas), collapse = ", ")))
  }

  # Write Out New Version ---------------------------------------------------
  write_csv_dots(
    x = .data,
    file = .file
  )

  # Prepare Metadata Folder -------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)

  # Create directory anew if it exists
  if (dir.exists(.meta_data_folder)) {
    unlink(.meta_data_folder, recursive = TRUE)
  }

  dir.create(.meta_data_folder)

  # Write Out Metadata ------------------------------------------------------
  haven::write_xpt(
    data = yspec::ys_add_labels(.data, .spec),
    path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
    version = 5, # Use version 5
    name = paste0("a", substr(gsub("[^[:alnum:]]", "", .data_name), 1, 7)) # Max of 8 chars
  )

  # Write out the spec
  .spec_list <-
    purrr::map(as.list(.spec), ~ {
      .x[intersect(c("short", "type", "unit", "values", "decode"), names(.x))]
    })

  yaml::write_yaml(.spec_list, file.path(.meta_data_folder, "spec-list.yml"))

  # Try to render spec
  silence_console_output(
    yspec::render_fda_define(
      x = .spec,
      stem = "define",
      output_dir = .meta_data_folder
    )
  )

  # Search for ID column
  if (!.subject_col %in% colnames(.data)) {
    stop("Defined .subject_col '", .subject_col, "' not found in data")
  }

  # Execute data diffs ------------------------------------------------------
  compare_df <- read_csv_dots(.file)

  if (!is.null(base_df_list$base_df) & .execute_diffs) {
    diffs <-
      execute_data_diffs(
        .base_df = base_df_list$base_df,
        .compare_df = compare_df,
        .subject_col = .subject_col,
        .base_from_svn = base_df_list$from_svn
      )

    write_csv_dots(
      x = diffs$diffs,
      file = file.path(.meta_data_folder, 'diffs.csv')
    )

    write_csv_dots(
      x = diffs$subject_diffs,
      file = file.path(.meta_data_folder, 'subject-diffs.csv')
    )
  }

  cli::cli_alert(paste0("File written: ", cli::col_blue(tools::file_path_as_absolute(.file))))
  cli::cli_alert(paste0("Metadata folder: ", cli::col_blue(tools::file_path_as_absolute(.meta_data_folder))))


  # Return ------------------------------------------------------------------
  if (.return_base_compare) {
    return(
      list(
        base_df = base_df_list$base_df,
        compare_df = compare_df,
        base_from_svn = base_df_list$from_svn
      )
    )

  } else {
    return(invisible(NULL))
  }

}
