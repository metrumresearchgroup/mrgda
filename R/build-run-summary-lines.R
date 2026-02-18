#' Format a name-value tibble as indented, aligned lines
#'
#' @param .df A two-column tibble with `name` and `value` columns.
#' @return A character vector of formatted lines.
#' @noRd
format_kv_lines <- function(.df) {
  max_width <- max(nchar(.df$name))
  paste0("    ", format(.df$name, width = max_width), "   ", .df$value)
}

#' Build summary lines for write_derived output
#'
#' @param .data_standard_rows A two-column tibble (`name`, `value`) of standard
#'   data diffs (Rows, Columns, Subjects).
#' @param .data_variable_rows A two-column tibble (`name`, `value`) of
#'   variable-specific data diffs (value changes, added/removed columns).
#' @param .spec_diff_rows A two-column tibble (`name`, `value`) of spec diffs.
#' @param .current_info Character string describing the local version
#'   (e.g. `"andersone at 2026-02-17 18:54:01"`).
#' @param .baseline_info Character string describing the repository version
#'   (e.g. `"r255 by andersone at 2026-01-28 11:45:21"`).
#'
#' @return A character vector of lines for console/file output.
#' @noRd
build_run_summary_lines <- function(
    .data_standard_rows,
    .data_variable_rows,
    .spec_diff_rows,
    .current_info,
    .baseline_info
) {
  rule <- paste(rep("=", 65), collapse = "")
  thin_rule <- paste(rep("-", 65), collapse = "")

  lbl_width <- nchar("Repository:")
  header <- c(
    rule,
    "  WRITE DERIVED SUMMARY",
    rule,
    paste0("  ", format("Local:", width = lbl_width), "  ", .current_info),
    paste0("  ", format("Repository:", width = lbl_width), "  ", .baseline_info),
    thin_rule
  )

  has_data_diffs <- nrow(.data_standard_rows) > 0 || nrow(.data_variable_rows) > 0

  body <- character()
  if (has_data_diffs) {
    body <- c(body, "", "  DATA CHANGES", format_kv_lines(.data_standard_rows))

    if (nrow(.data_variable_rows) > 0) {
      body <- c(body, "", "  VARIABLE CHANGES", format_kv_lines(.data_variable_rows))
    }
  } else {
    body <- c(body, "", "  DATA CHANGES", "    No data diffs found.")
  }

  if (nrow(.spec_diff_rows) > 0) {
    body <- c(body, "", "  SPEC CHANGES", format_kv_lines(.spec_diff_rows))
  } else {
    body <- c(body, "", "  SPEC CHANGES", "    No spec diffs found.")
  }

  c(header, body, "", rule)
}
