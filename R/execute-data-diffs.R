#' Execute Differences Between Data Frames
#'
#' This function executes the comparison of two data frames, generates
#' the differences, and writes them to specified output files.
#'
#' @param .base_df A data frame that serves as the base for comparison.
#' @param .compare_df A data frame to compare against the base data frame.
#' @param .subject_col A string representing the column name to be used for subject-based differences.
#'        Set to `NULL` for data frames not containing an subject column.
#' @param .base_from_svn Logical. Was base df exported from svn?
#' @param .print_output Logical. Should console output be printed?
#'
#' @return No return value, but this function will write files to the specified output directory
#'         containing the differences between the input data frames.
#'
#' @export
execute_data_diffs <- function(.base_df, .compare_df, .subject_col, .base_from_svn = FALSE, .print_output = TRUE){

  # Exit if no names in common ----------------------------------------------
  names_in_common <- dplyr::intersect(names(.base_df), names(.compare_df))

  if (length(names_in_common) == 0) {
    stop("The base and compare data frames do not have any columns to compare")
  }

  out <- list()

  # Initialize to empty data frames
  out$diffs <- tibble::tribble(~name, ~value)
  out$standard_diffs <- tibble::tibble(name = character(), value = character())
  out$variable_diffs <- tibble::tibble(name = character(), value = character())

  # Diffs across entire data ------------------------------------------------
  full_diff <- suppressMessages(
    diffdf::diffdf(
      base = .base_df,
      compare = .compare_df,
      suppress_warnings = TRUE,
      strict_numeric = FALSE,
      strict_factor = FALSE
    )
  )

  if (length(full_diff) == 0) {
    if (.print_output) {
      cli::cli_alert_info("No diffs since last version found")
    }
    return(out)
  }

  if (.print_output) {
    cli::cli_alert_info("Diffs since last version:")
  }

  # ── Standard summary rows (Rows, Columns, Subjects) ─────────────────────

  n_row_diff <- nrow(.compare_df) - nrow(.base_df)
  n_row_diff_msg <- dplyr::case_when(
    n_row_diff == 0 ~ "No change",
    n_row_diff < 0 ~ paste0(gsub("-", "", as.character(n_row_diff), fixed=TRUE), " removed"),
    n_row_diff > 0 ~ paste0(n_row_diff, " added")
  )

  n_col_diff <- ncol(.compare_df) - ncol(.base_df)
  n_col_diff_msg <- dplyr::case_when(
    n_col_diff == 0 ~ "No change",
    n_col_diff < 0 ~ paste0(gsub("-", "", as.character(n_col_diff), fixed=TRUE), " removed"),
    n_col_diff > 0 ~ paste0(n_col_diff, " added")
  )

  standard_diffs <- tibble::tibble(
    name = c("Rows", "Columns"),
    value = c(n_row_diff_msg, n_col_diff_msg)
  )

  if (!is.null(.subject_col)) {
    datas_have_id <- (.subject_col %in% names(.base_df)) &&
      (.subject_col %in% names(.compare_df))

    if (!datas_have_id) {
      stop(glue::glue(
        "The specified `.subject_col` ({.subject_col}) is not present in one or both of the data frames"
      ))
    }

    n_id_diff <- length(unique(.compare_df[[.subject_col]])) -
      length(unique(.base_df[[.subject_col]]))
    n_id_diff_msg <- dplyr::case_when(
      n_id_diff == 0 ~ "No change",
      n_id_diff < 0 ~ paste0(gsub("-", "", as.character(n_id_diff), fixed=TRUE), " removed"),
      n_id_diff > 0 ~ paste0(n_id_diff, " added")
    )

    standard_diffs <- dplyr::bind_rows(
      standard_diffs,
      tibble::tibble(name = "Subjects", value = n_id_diff_msg)
    )
  }

  # ── Variable-specific rows ─────────────────────────────────────────────

  variable_diffs <- tibble::tibble(name = character(), value = character())

  if (!is.null(full_diff$ExtColsBase)) {
    variable_diffs <- dplyr::bind_rows(
      variable_diffs,
      tibble::tibble(
        name = "Removed Columns",
        value = paste(full_diff$ExtColsBase$COLUMNS, collapse = ", ")
      )
    )
  }

  if (!is.null(full_diff$ExtColsComp)) {
    variable_diffs <- dplyr::bind_rows(
      variable_diffs,
      tibble::tibble(
        name = "New Columns",
        value = paste(full_diff$ExtColsComp$COLUMNS, collapse = ", ")
      )
    )
  }

  if (!is.null(full_diff$NumDiff)) {
    variable_diffs <- dplyr::bind_rows(
      variable_diffs,
      full_diff$NumDiff %>%
        dplyr::transmute(name = Variable, value = paste0(`No of Differences`, " diffs"))
    )
  }

  print_diffs <- dplyr::bind_rows(standard_diffs, variable_diffs)
  out$diffs <- print_diffs
  out$standard_diffs <- standard_diffs
  out$variable_diffs <- variable_diffs

  out$value_diffs <- diffdf_value_changes_to_df(full_diff)

  if (.print_output) {
    print(
      cli::boxx(
        padding = 0,
        knitr::kable(
          x = print_diffs,
          align = 'c',
          format = "simple"
        )
      )
    )
  }

  return(out)
}
