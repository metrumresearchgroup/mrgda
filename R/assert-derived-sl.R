# Keep duplicate-value messages short and consistent across the assertions.
format_derived_issue_values <- function(.values, .limit = 5L) {
  values <- unique(as.character(.values))
  shown <- utils::head(values, .limit)
  suffix <- if (length(values) > .limit) ", ..." else ""
  paste0(paste(shown, collapse = ", "), suffix)
}

#' Assert the structure of a subject-level derived-data list
#'
#' @description
#' Validate the complete `derived$sl` list before its elements are joined.
#' Each element must be a nonempty, named, ungrouped data frame containing
#' exactly `USUBJID` and one model variable, with no missing or duplicated
#' subject identifiers. The element name must match its model variable without
#' regard to case.
#'
#' This is a structural assertion. It does not check model-variable missingness,
#' subject coverage across elements, allowed values, derivation correctness, or
#' scientific plausibility.
#'
#' @param .sl The complete subject-level list, typically `derived$sl`.
#' @param .subject_col A single character string naming the subject identifier.
#'   Defaults to `"USUBJID"`.
#'
#' @return `.sl`, invisibly. The function aborts with all detected structural
#'   issues when validation fails.
#'
#' @examples
#' sl <- list(
#'   sex = tibble::tibble(USUBJID = c("01", "02"), SEX = c(1, 2)),
#'   blwt = tibble::tibble(USUBJID = c("01", "02"), BLWT = c(70, 80))
#' )
#'
#' assert_derived_sl(sl)
#'
#' @export
assert_derived_sl <- function(.sl, .subject_col = "USUBJID") {
  # The top-level object must be a list, not a single data frame.
  if (!is.list(.sl) || is.data.frame(.sl)) {
    cli::cli_abort("{.arg .sl} must be a named list of data frames")
  }

  # The subject column must be identified by one usable column name.
  if (
    !is.character(.subject_col) ||
      length(.subject_col) != 1 ||
      is.na(.subject_col) ||
      !nzchar(.subject_col)
  ) {
    cli::cli_abort("{.arg .subject_col} must be a single non-empty string")
  }

  # An empty list cannot produce a subject-level dataset.
  if (length(.sl) == 0) {
    cli::cli_abort("{.arg .sl} must contain at least one subject-level element")
  }

  # Stable, unique names identify the model variable in each element.
  sl_names <- names(.sl)
  if (
    is.null(sl_names) ||
      anyNA(sl_names) ||
      any(!nzchar(sl_names)) ||
      anyDuplicated(tolower(sl_names))
  ) {
    cli::cli_abort(
      "{.arg .sl} must have unique, non-empty, case-insensitive names"
    )
  }

  # Inspect every element and report all structural problems in one error.
  issues <-
    purrr::imap(.sl, function(piece, piece_name) {
      label <- paste0("derived$sl$", piece_name)
      piece_issues <- character()

      # Every element must be a data frame before column checks are safe.
      if (!is.data.frame(piece)) {
        return(paste0(label, " must be a data frame"))
      }

      # Grouping can silently change later joins and summaries.
      if (dplyr::is_grouped_df(piece)) {
        piece_issues <- c(
          piece_issues,
          paste0(label, " must be ungrouped")
        )
      }

      # Each model variable must contribute at least one subject row.
      if (nrow(piece) == 0) {
        piece_issues <- c(
          piece_issues,
          paste0(label, " must contain at least one row")
        )
      }

      # Duplicate column names make name-based selection ambiguous.
      duplicate_columns <- anyDuplicated(names(piece)) > 0
      if (duplicate_columns) {
        piece_issues <- c(
          piece_issues,
          paste0(label, " must have unique column names")
        )
      }

      # List columns cannot be ordinary model-data fields.
      list_columns <- names(piece)[purrr::map_lgl(piece, is.list)]
      if (length(list_columns) > 0) {
        piece_issues <- c(
          piece_issues,
          paste0(
            label,
            " must not contain list column(s): ",
            paste(list_columns, collapse = ", ")
          )
        )
      }

      # Every subject-level element needs the subject join key.
      if (!(.subject_col %in% names(piece))) {
        piece_issues <- c(
          piece_issues,
          paste0(label, " must contain ", .subject_col)
        )
      }

      # Keep each element narrow: one subject key plus one model variable.
      model_variables <- setdiff(names(piece), .subject_col)
      if (length(model_variables) != 1) {
        piece_issues <- c(
          piece_issues,
          paste0(
            label,
            " must contain exactly ",
            .subject_col,
            " and one model variable"
          )
        )
      } else if (tolower(piece_name) != tolower(model_variables)) {
        piece_issues <- c(
          piece_issues,
          paste0(
            label,
            " must be named for model variable ",
            model_variables
          )
        )
      }

      # Check subject values only when the subject column is unambiguous and
      # atomic; earlier checks explain why unusable columns fail.
      if (
        !duplicate_columns &&
          .subject_col %in% names(piece) &&
          !is.list(piece[[.subject_col]])
      ) {
        subject_values <- piece[[.subject_col]]
        missing_subject <-
          is.na(subject_values) |
          !nzchar(trimws(as.character(subject_values)))

        # Missing or blank subjects cannot participate in the final join.
        if (any(missing_subject)) {
          piece_issues <- c(
            piece_issues,
            paste0(
              label,
              " has ",
              sum(missing_subject),
              " missing or blank ",
              .subject_col,
              " value(s)"
            )
          )
        }

        # A subject may appear only once within a subject-level element.
        duplicate_subjects <-
          piece %>%
          dplyr::ungroup() %>%
          dplyr::filter(!missing_subject) %>%
          dplyr::count(.data[[.subject_col]], name = ".n") %>%
          dplyr::filter(.data$.n > 1) %>%
          dplyr::pull(.data[[.subject_col]])

        if (length(duplicate_subjects) > 0) {
          piece_issues <- c(
            piece_issues,
            paste0(
              label,
              " has duplicated ",
              .subject_col,
              " value(s): ",
              format_derived_issue_values(duplicate_subjects)
            )
          )
        }
      }

      piece_issues
    }) %>%
    purrr::flatten_chr()

  # Return one readable error containing every issue found above.
  if (length(issues) > 0) {
    cli::cli_abort(c(
      "{.arg .sl} failed subject-level structure checks:",
      stats::setNames(issues, rep("x", length(issues)))
    ))
  }

  # Preserve the input so this assertion can be used in a pipeline.
  invisible(.sl)
}
