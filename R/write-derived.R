#' Write a derived data set and corresponding metadata
#'
#' @description
#' This function will take a data frame in R and write it out to csv.
#' It also creates a metadata folder, storing the xpt file along with other useful information.
#'
#' The csv and spec-list.yml are always written. The xpt, define document, and
#' latest-data-diff.txt are only regenerated when the csv or spec-list.yml
#' content has changed, avoiding unnecessary diffs in version control from
#' embedded timestamps.
#'
#' If a legacy metadata folder containing diffs.csv is detected, the metadata
#' folder is removed and recreated before writing new outputs.
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

  # Prepare Metadata Folder -------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)
  .legacy_diffs_file <- file.path(.meta_data_folder, "diffs.csv")

  if (dir.exists(.meta_data_folder) && file.exists(.legacy_diffs_file)) {
    cli::cli_alert_warning(c(
      paste0("Legacy metadata detected in ", .meta_data_folder, "."),
      "This metadata folder was created by previous `write_derived()` functionality (`diffs.csv` detected).",
      "The metadata folder will be removed and recreated for backward compatibility."
    ))
    unlink(.meta_data_folder, recursive = TRUE)
  }

  # Capture checksums before any writes (for skip-if-unchanged logic)
  old_csv_md5 <- if (file.exists(.file)) {
    unname(tools::md5sum(.file))
  }
  old_spec_md5 <- if (file.exists(file.path(.meta_data_folder, "spec-list.yml"))) {
    unname(tools::md5sum(file.path(.meta_data_folder, "spec-list.yml")))
  }

  # Write Out New Version ---------------------------------------------------
  write_csv_dots(
    x = .data,
    file = .file
  )

  if (!dir.exists(.meta_data_folder)) {
    dir.create(.meta_data_folder)
  }

  .spec_list <-
    purrr::map(as.list(.spec), ~ {
      .x[intersect(c("short", "type", "unit", "values", "decode"), names(.x))]
    })

  yaml::write_yaml(.spec_list, file.path(.meta_data_folder, "spec-list.yml"))

  # Skip XPT/define writes if CSV and spec are unchanged (avoids SVN diffs from timestamps)
  .needs_update <- is.null(old_csv_md5) | is.null(old_spec_md5) |
    !identical(old_csv_md5, unname(tools::md5sum(.file))) |
    !identical(old_spec_md5, unname(tools::md5sum(file.path(.meta_data_folder, "spec-list.yml"))))

  if (.needs_update) {
    # Write Out Metadata ------------------------------------------------------
    haven::write_xpt(
      data = yspec::ys_add_labels(.data, .spec),
      path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
      version = 5, # Use version 5
      name = paste0("a", substr(gsub("[^[:alnum:]]", "", .data_name), 1, 7)) # Max of 8 chars
    )

    # Try to render spec
    silence_console_output(
      yspec::render_fda_define(
        x = .spec,
        stem = "define",
        output_dir = .meta_data_folder
      )
    )
  }

  # Search for ID column
  if (!.subject_col %in% colnames(.data)) {
    stop("Defined .subject_col '", .subject_col, "' not found in data")
  }

  # Execute data diffs ------------------------------------------------------
  compare_df <- read_csv_dots(.file)

  if (!is.null(base_df_list$base_df) & .execute_diffs & .needs_update) {
    diffs <-
      execute_data_diffs(
        .base_df = base_df_list$base_df,
        .compare_df = compare_df,
        .subject_col = .subject_col,
        .base_from_svn = base_df_list$from_svn
      )

    if (nrow(diffs$diffs) > 0) {
      diff_table <- knitr::kable(
        x = diffs$diffs,
        format = "simple"
      )
      writeLines(
        text = c(
          paste0("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
          paste0("Generated by: ", Sys.info()[["user"]]),
          "",
          diff_table
        ),
        con = file.path(.meta_data_folder, "latest-data-diff.txt")
      )
    }
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
