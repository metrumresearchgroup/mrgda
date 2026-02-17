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
#' If a legacy metadata folder containing diffs.csv is detected, an error is
#' raised asking the user to delete the folder before re-running.
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
    cli::cli_abort("{.arg .file} must reference a {.val csv} file")
  }

  # Ensure data conforms to spec before writing anything
  spec_check <- yspec::ys_check(.data, .spec, error_on_fail = FALSE) %>%
    suppressMessages()

  if (!spec_check) {
    cli::cli_abort(
      "Spec check failed. Please run {.fn yspec::ys_check} and ensure it passes."
    )
  }

  if (!.subject_col %in% colnames(.data)) {
    cli::cli_abort(
      "Defined {.arg .subject_col} {.val {.subject_col}} not found in data"
    )
  }

  # Commas in values would corrupt the csv output
  check_for_commas <- purrr::map(
    .data,
    ~ any(stringr::str_detect(string = .x, pattern = ","), na.rm = TRUE)
  )

  check_for_commas <- check_for_commas[unlist(check_for_commas)]
  if (length(check_for_commas) > 0) {
    cli::cli_abort(
      "Comma found in following column(s): {.val {names(check_for_commas)}}"
    )
  }

  # Retrieve the previous version of the data for diffing later.
  # Uses svn or local copy depending on .compare_from_svn.
  if (is.null(.prev_file)) .prev_file <- .file
  base_df_list <- get_svn_baseline(.prev_file, .compare_from_svn)

  # Derive paths -------------------------------------------------------------
  # Metadata lives in a folder named after the csv (e.g. pk.csv -> pk/)
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)
  .spec_list_file <- file.path(.meta_data_folder, "spec-list.yml")

  # Legacy check -------------------------------------------------------------
  # Older versions of this package stored diffs in a diffs.csv file.
  # That format is no longer supported.
  .legacy_diffs_file <- file.path(.meta_data_folder, "diffs.csv")
  if (dir.exists(.meta_data_folder) && file.exists(.legacy_diffs_file)) {
    .abs_meta <- tools::file_path_as_absolute(.meta_data_folder)
    cli::cli_abort(c(
      "The metadata folder uses an outdated format that is no longer supported.",
      "i" = "Please delete it and re-run {.fn write_derived}.",
      " " = '{.code fs::dir_delete("{(.abs_meta)}")}'
    ))
  }

  # Retrieve the previous version of the spec for diffing later.
  # Uses svn or local copy depending on .compare_from_svn.
  base_spec_list <- get_svn_baseline(
    .prev_file = .spec_list_file,
    .compare_from_svn = .compare_from_svn,
    .reader = yaml::read_yaml,
    .file_ext = ".yml"
  )

  # Capture old state before any writes --------------------------------------
  # Snapshot md5 checksums so we can detect changes after writing.
  # NULL md5 means the file didn't exist yet (first run).
  old_csv_md5 <- if (file.exists(.file)) {
    unname(tools::md5sum(.file))
  }
  old_spec_md5 <- if (file.exists(.spec_list_file)) {
    unname(tools::md5sum(.spec_list_file))
  }

  # Write csv and spec-list (always) -----------------------------------------
  write_csv_dots(
    x = .data,
    file = .file
  )

  if (!dir.exists(.meta_data_folder)) {
    dir.create(.meta_data_folder)
  }

  # Extract only the spec fields we want to persist
  .spec_list <- purrr::map(
    as.list(.spec),
    ~ {
      .x[intersect(c("short", "type", "unit", "values", "decode"), names(.x))]
    }
  )

  yaml::write_yaml(.spec_list, .spec_list_file)

  # Determine if csv or spec changed -----------------------------------------
  # Compare md5 checksums before and after writing. If either file is new
  # (NULL md5) or its content changed, downstream artifacts need regenerating.
  .needs_update <-
    is.null(old_csv_md5) |
    is.null(old_spec_md5) |
    !identical(old_csv_md5, unname(tools::md5sum(.file))) |
    !identical(old_spec_md5, unname(tools::md5sum(.spec_list_file)))

  # Write xpt and define (only when csv or spec changed) ---------------------
  # These files contain embedded timestamps, so we only regenerate them when
  # actual content changed to keep version control diffs clean.
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
  # Compare old vs new spec-list for added/removed/updated fields
  spec_diff_rows <- tibble::tibble(name = character(), value = character())
  if (!is.null(base_spec_list$base_df)) {
    spec_diff_rows <- execute_spec_diffs(
      .base_spec = base_spec_list$base_df,
      .compare_spec = yaml::read_yaml(.spec_list_file)
    )$diffs
  }

  # Compare old vs new data for row/column/value changes
  compare_df <- read_csv_dots(.file)

  data_diff_rows <- tibble::tibble(name = character(), value = character())
  if (!is.null(base_df_list$base_df) && .execute_diffs && .needs_update) {
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
  # Print and save a human-readable summary of what changed. Only written
  # when there are actual diffs to report.
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
