#' Write a derived data set and corresponding metadata
#'
#' @description
#' This function will take a data frame in R and write it out to csv.
#' It also creates a metadata folder, storing the xpt file along with other useful information.
#'
#' The csv and spec-list.yml are always written. The xpt, define document, and
#' last-run-summary.txt are only regenerated when the csv or spec-list.yml
#' content has changed, avoiding unnecessary diffs in version control from
#' embedded timestamps.
#'
#' last-run-summary.txt includes separate sections for data changes and
#' spec-list changes (added/removed variables and updated spec fields).
#' The summary is printed to the console and written to last-run-summary.txt
#' only when at least one data or spec diff exists.
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
write_derived <- function(
  .data,
  .spec,
  .file,
  .subject_col = "ID",
  .prev_file = NULL,
  .compare_from_svn = TRUE,
  .return_base_compare = FALSE,
  .execute_diffs = TRUE
) {
  # Input validation ---------------------------------------------------------
  if (tools::file_ext(.file) != "csv") {
    stop("'.file' must reference a 'csv' file")
  }

  spec_check <- yspec::ys_check(.data, .spec, error_on_fail = FALSE) %>%
    suppressMessages()

  if (!spec_check) {
    stop(
      "Spec check failed. Please run 'yspec::ys_check()' and ensure it passes.",
      call. = FALSE
    )
  }

  if (!.subject_col %in% colnames(.data)) {
    stop("Defined .subject_col '", .subject_col, "' not found in data")
  }

  check_for_commas <- purrr::map(
    .data,
    ~ any(stringr::str_detect(string = .x, pattern = ","), na.rm = TRUE)
  )

  check_for_commas <- check_for_commas[unlist(check_for_commas)]
  if (length(check_for_commas) > 0) {
    cli::cli_abort(paste0(
      "Comma found in following column(s):",
      paste(names(check_for_commas), collapse = ", ")
    ))
  }

  # Base version for data diff -----------------------------------------------
  .prev_file <- ifelse(is.null(.prev_file), .file, .prev_file)
  base_df_list <- get_base_df(.prev_file, .compare_from_svn)

  # Derive paths -------------------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)
  .spec_list_file <- file.path(.meta_data_folder, "spec-list.yml")

  # Capture old state before any writes --------------------------------------
  # NULL md5 means the file did not exist, which forces a full update.
  old_csv_md5 <- if (file.exists(.file)) {
    unname(tools::md5sum(.file))
  }
  old_spec_md5 <- if (file.exists(.spec_list_file)) {
    unname(tools::md5sum(.spec_list_file))
  }

  # Old spec content for spec diffing (must happen before legacy migration
  # or spec rewrite).
  old_spec_list <- if (file.exists(.spec_list_file)) {
    yaml::read_yaml(.spec_list_file)
  }
  if (is.null(old_spec_list)) {
    old_spec_list <- list()
  }

  # Legacy migration ---------------------------------------------------------
  # One-time migration from `diffs.csv` metadata layout.
  ### --- LEGACY CODE START
  .legacy_diffs_file <- file.path(.meta_data_folder, "diffs.csv")
  legacy_recreated <- FALSE
  if (dir.exists(.meta_data_folder) && file.exists(.legacy_diffs_file)) {
    cli::cli_alert_warning(
      "Legacy metadata format detected; recreating metadata folder."
    )
    unlink(.meta_data_folder, recursive = TRUE)
    legacy_recreated <- TRUE
  }
  ### --- LEGACY CODE END

  # Write csv and spec-list (always) -----------------------------------------
  write_csv_dots(
    x = .data,
    file = .file
  )

  if (!dir.exists(.meta_data_folder)) {
    dir.create(.meta_data_folder)
  }

  .spec_list <- purrr::map(
    as.list(.spec),
    ~ {
      .x[intersect(c("short", "type", "unit", "values", "decode"), names(.x))]
    }
  )

  yaml::write_yaml(.spec_list, .spec_list_file)

  # Determine if csv or spec changed -----------------------------------------
  .needs_update <-
    legacy_recreated | ### LEGACY
    is.null(old_csv_md5) |
    is.null(old_spec_md5) |
    !identical(old_csv_md5, unname(tools::md5sum(.file))) |
    !identical(old_spec_md5, unname(tools::md5sum(.spec_list_file)))

  # Write xpt and define (only when csv or spec changed) ---------------------
  if (.needs_update) {
    haven::write_xpt(
      data = yspec::ys_add_labels(.data, .spec),
      path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
      version = 5,
      name = paste0("a", substr(gsub("[^[:alnum:]]", "", .data_name), 1, 7))
    )

    silence_console_output(
      yspec::render_fda_define(
        x = .spec,
        stem = "define",
        output_dir = .meta_data_folder
      )
    )
  }

  # Execute diffs ------------------------------------------------------------
  spec_diff_rows <- tibble::tibble(name = character(), value = character())
  if (!is.null(old_spec_md5)) {
    spec_diff_rows <- execute_spec_diffs(
      .base_spec = old_spec_list,
      .compare_spec = yaml::read_yaml(.spec_list_file)
    )$diffs
  }

  compare_df <- read_csv_dots(.file)

  data_diff_rows <- tibble::tibble(name = character(), value = character())
  if (!is.null(base_df_list$base_df) & .execute_diffs & .needs_update) {
    diffs <- execute_data_diffs(
      .base_df = base_df_list$base_df,
      .compare_df = compare_df,
      .subject_col = .subject_col,
      .base_from_svn = base_df_list$from_svn,
      .print_output = FALSE
    )
    data_diff_rows <- diffs$diffs
  }

  # Write summary ------------------------------------------------------------
  has_summary_diffs <- nrow(data_diff_rows) > 0 || nrow(spec_diff_rows) > 0
  if (has_summary_diffs) {
    summary_lines <- build_run_summary_lines(
      .data_diff_rows = data_diff_rows,
      .spec_diff_rows = spec_diff_rows
    )

    writeLines(summary_lines)

    writeLines(
      text = summary_lines,
      con = file.path(.meta_data_folder, "last-run-summary.txt")
    )

    writeLines("")
  } else {
    cli::cli_alert_info(
      "No data/spec diffs detected; last-run-summary.txt not updated."
    )
  }

  cli::cli_alert(paste0(
    "File written: ",
    cli::col_blue(tools::file_path_as_absolute(.file))
  ))
  cli::cli_alert(paste0(
    "Metadata folder: ",
    cli::col_blue(tools::file_path_as_absolute(.meta_data_folder))
  ))

  # Return -------------------------------------------------------------------
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
