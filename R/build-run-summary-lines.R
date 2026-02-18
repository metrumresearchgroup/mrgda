#' Format a name-value tibble as indented, aligned lines
#'
#' @param .df A two-column tibble with `name` and `value` columns.
#' @return A character vector of formatted lines.
#' @noRd
format_kv_lines <- function(.df) {
  max_width <- max(nchar(.df$name))
  paste0("    ", format(.df$name, width = max_width), "   ", .df$value)
}

#' Print a name-value tibble to console with cli styling
#'
#' Values containing `"No change"` are dimmed; all other values are highlighted.
#'
#' @param .df A two-column tibble with `name` and `value` columns.
#' @noRd
print_styled_kv <- function(.df) {
  max_width <- max(nchar(.df$name))
  for (i in seq_len(nrow(.df))) {
    padded_name <- format(.df$name[i], width = max_width)
    val <- .df$value[i]
    if (grepl("No change", val, fixed = TRUE)) {
      styled_val <- cli::style_dim(val)
    } else {
      styled_val <- cli::style_bold(cli::col_cyan(val))
    }
    cat(paste0("    ", padded_name, "   ", styled_val, "\n"))
  }
}

#' Build summary lines for write_derived output
#'
#' @param .data_standard_rows A two-column tibble (`name`, `value`) of standard
#'   data diffs (Rows, Columns, Subjects).
#' @param .data_variable_rows A two-column tibble (`name`, `value`) of
#'   variable-specific data diffs (value changes, added/removed columns).
#' @param .spec_diff_rows A two-column tibble (`name`, `value`) of spec diffs.
#' @param .current_info Character string describing the current run
#'   (e.g. `"local by anderson at 2026-02-17 18:54:01"`).
#' @param .baseline_info Character string describing the baseline
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

  lbl_width <- nchar("Comparing against:")
  header <- c(
    rule,
    "  WRITE DERIVED SUMMARY",
    rule,
    paste0("  ", format("Current:", width = lbl_width), "  ", .current_info),
    paste0("  ", format("Comparing against:", width = lbl_width), "  ", .baseline_info),
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
    body <- c(body, "", "  DATA CHANGES", "    No data diffs detected.")
  }

  if (nrow(.spec_diff_rows) > 0) {
    body <- c(body, "", "  SPEC CHANGES", format_kv_lines(.spec_diff_rows))
  } else {
    body <- c(body, "", "  SPEC CHANGES", "    No spec diffs detected.")
  }

  c(header, body, "", rule)
}
