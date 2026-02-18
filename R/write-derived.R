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

  # Flow:
  #   1. Validate inputs
  #   2. Derive paths
  #   3. Snapshot old state (baselines + md5s, before writing anything)
  #   4. Write csv and spec-list (always)
  #   5. Regenerate xpt and define (only when content changed)
  #   6. Compute and report diffs
  #   7. Return

  # ── 1. Validate inputs ────────────────────────────────────────────────────

  if (tools::file_ext(.file) != "csv") {
    cli::cli_abort("{.arg .file} must reference a {.val csv} file")
  }

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

  # ── 2. Derive paths ───────────────────────────────────────────────────────

  if (is.null(.prev_file)) {
    .prev_file <- .file
  }
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)
  .spec_list_file <- file.path(.meta_data_folder, "spec-list.yml")

  # Guard against legacy metadata format (diffs.csv)
  .legacy_diffs_file <- file.path(.meta_data_folder, "diffs.csv")
  if (dir.exists(.meta_data_folder) && file.exists(.legacy_diffs_file)) {
    .abs_meta <- tools::file_path_as_absolute(.meta_data_folder)
    cli::cli_abort(c(
      "The metadata folder uses an outdated format that is no longer supported.",
      "i" = "Please delete it and re-run {.fn write_derived}.",
      " " = '{.code fs::dir_delete("{(.abs_meta)}")}'
    ))
  }

  # ── 3. Snapshot old state ──────────────────────────────────────────────────
  # Everything here runs BEFORE any files are written so we capture the
  # previous version of both data and spec for diffing, plus md5 checksums
  # to detect whether content actually changed after writing.

  # Baselines for diffing (from SVN or local, depending on .compare_from_svn)
  base_df_list <- get_svn_baseline(.prev_file, .compare_from_svn)
  base_spec_list <- get_svn_baseline(
    .prev_file = .spec_list_file,
    .compare_from_svn = .compare_from_svn,
    .reader = yaml::read_yaml,
    .file_ext = ".yml"
  )

  # md5 checksums (NULL = file doesn't exist yet, i.e. first run)
  old_csv_md5 <- if (file.exists(.file)) {
    unname(tools::md5sum(.file))
  }
  old_spec_md5 <- if (file.exists(.spec_list_file)) {
    unname(tools::md5sum(.spec_list_file))
  }

  # ── 4. Write csv and spec-list (always) ────────────────────────────────────

  write_csv_dots(x = .data, file = .file)

  if (!dir.exists(.meta_data_folder)) {
    dir.create(.meta_data_folder)
  }

  .spec_list <- purrr::map(
    as.list(.spec),
    ~ .x[intersect(c("short", "type", "unit", "values", "decode"), names(.x))]
  )

  yaml::write_yaml(.spec_list, .spec_list_file)

  # ── 5. Regenerate xpt and define (only when content changed) ───────────────
  # These files embed timestamps, so we skip regeneration when nothing changed
  # to keep version-control diffs clean.

  .needs_update <-
    is.null(old_csv_md5) |
    is.null(old_spec_md5) |
    !identical(old_csv_md5, unname(tools::md5sum(.file))) |
    !identical(old_spec_md5, unname(tools::md5sum(.spec_list_file)))

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

  # ── 6. Compute and report diffs ────────────────────────────────────────────

  # Spec and data diffs: run both whenever content changed
  spec_diff_rows <- tibble::tibble(name = character(), value = character())
  data_diff_rows <- tibble::tibble(name = character(), value = character())
  data_standard_rows <- tibble::tibble(name = character(), value = character())
  data_variable_rows <- tibble::tibble(name = character(), value = character())
  compare_df <- read_csv_dots(.file)

  if (.execute_diffs && .needs_update) {
    if (!is.null(base_spec_list$base_df)) {
      spec_diffs <- execute_spec_diffs(
        .base_spec = base_spec_list$base_df,
        .compare_spec = yaml::read_yaml(.spec_list_file)
      )
      spec_diff_rows <- spec_diffs$diffs
    }

    if (!is.null(base_df_list$base_df)) {
      data_diffs <- execute_data_diffs(
        .base_df = base_df_list$base_df,
        .compare_df = compare_df,
        .subject_col = .subject_col,
        .print_output = FALSE
      )
      data_diff_rows <- data_diffs$diffs
      data_standard_rows <- data_diffs$standard_diffs
      data_variable_rows <- data_diffs$variable_diffs
    }
  }

  # Print and write summary (only when there are actual diffs)
  .abs_file <- tools::file_path_as_absolute(.file)
  has_summary_diffs <- nrow(data_diff_rows) > 0 || nrow(spec_diff_rows) > 0

  if (has_summary_diffs) {
    generated_at <- Sys.time()
    generated_by <- Sys.info()[["user"]]
    generated_at_fmt <- format(generated_at, "%Y-%m-%d %H:%M:%S")

    current_info <- paste0("local by ", generated_by, " at ", generated_at_fmt)

    baseline_info <- if (base_df_list$from_svn) {
      parts <- paste0("r", base_df_list$prev_rev)
      if (!is.na(base_df_list$svn_author)) {
        parts <- paste0(parts, " by ", base_df_list$svn_author)
      }
      if (!is.na(base_df_list$svn_date)) {
        svn_date_clean <- sub(" [+-]\\d{4}$", "", base_df_list$svn_date)
        parts <- paste0(parts, " at ", svn_date_clean)
      }
      parts
    } else {
      paste0("local by ", generated_by)
    }

    summary_lines <- build_run_summary_lines(
      .data_standard_rows = data_standard_rows,
      .data_variable_rows = data_variable_rows,
      .spec_diff_rows = spec_diff_rows,
      .current_info = current_info,
      .baseline_info = baseline_info
    )

    cat("\n")
    writeLines(summary_lines)
    cat(paste0("  File written: ", .abs_file, "\n"))

    writeLines(
      text = summary_lines,
      con = file.path(.meta_data_folder, "last-run-summary.txt")
    )
  } else {
    cat("\n")
    cat("No data/spec diffs detected; last-run-summary.txt not updated.\n")
    cat(paste0("File written: ", .abs_file, "\n"))
  }

  # ── 7. Return ──────────────────────────────────────────────────────────────

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
