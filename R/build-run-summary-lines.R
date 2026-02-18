#' Format a name-value tibble as indented, aligned lines
#'
#' @param .df A two-column tibble with `name` and `value` columns.
#' @return A character vector of formatted lines.
#' @noRd
format_kv_lines <- function(.df) {
  max_width <- max(nchar(.df$name))
  paste0("  ", format(.df$name, width = max_width), "   ", .df$value)
}

#' Print a name-value tibble to console with cli styling
#'
#' Values of `"No change"` are dimmed; all other values are highlighted.
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
    cat(paste0("  ", padded_name, "   ", styled_val, "\n"))
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
  summary_lines <- c(
    paste0("Current:            ", .current_info),
    paste0("Comparing against:  ", .baseline_info)
  )

  has_data_diffs <- nrow(.data_standard_rows) > 0 || nrow(.data_variable_rows) > 0

  if (has_data_diffs) {
    summary_lines <- c(summary_lines, "", "Data changes:", format_kv_lines(.data_standard_rows))

    if (nrow(.data_variable_rows) > 0) {
      summary_lines <- c(summary_lines, "", "Variable changes:", format_kv_lines(.data_variable_rows))
    }
  } else {
    summary_lines <- c(summary_lines, "", "Data changes:", "No data diffs detected.")
  }

  if (nrow(.spec_diff_rows) > 0) {
    summary_lines <- c(summary_lines, "", "Spec changes:", format_kv_lines(.spec_diff_rows))
  } else {
    summary_lines <- c(summary_lines, "", "Spec changes:", "No spec diffs detected.")
  }

  summary_lines
}
