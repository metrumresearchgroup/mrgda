#' Assert the structure of a time-varying derived-data list
#'
#' @description
#' Validate the complete `derived$tv` list before its elements are row-bound.
#' Each element must be a nonempty, named, ungrouped data frame with populated
#' `USUBJID` and `DATETIME` columns. Those two columns must uniquely identify a
#' row within each element, and columns shared across elements must have types
#' compatible with [dplyr::bind_rows()].
#'
#' Key uniqueness is checked within each element, not across elements. Events
#' from different elements may legitimately occur for the same subject at the
#' same datetime. This structural assertion does not validate event semantics,
#' allowed values, derivation correctness, or scientific plausibility.
#'
#' @param .tv The complete time-varying list, typically `derived$tv`.
#' @param .subject_col A single character string naming the subject identifier.
#'   Defaults to `"USUBJID"`.
#' @param .datetime_col A single character string naming the event datetime.
#'   Defaults to `"DATETIME"`.
#'
#' @return `.tv`, invisibly. The function aborts with all detected structural
#'   issues when validation fails.
#'
#' @examples
#' tv <- list(
#'   dosing = tibble::tibble(
#'     USUBJID = c("01", "02"),
#'     DATETIME = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"),
#'     EVID = 1,
#'     AMT = c(100, 100)
#'   ),
#'   pc = tibble::tibble(
#'     USUBJID = c("01", "02"),
#'     DATETIME = as.POSIXct(c("2024-01-01 01:00", "2024-01-02 01:00"), tz = "UTC"),
#'     EVID = 0,
#'     DV = c(10, 20)
#'   )
#' )
#'
#' assert_derived_tv(tv)
#'
#' @export
assert_derived_tv <- function(
  .tv,
  .subject_col = "USUBJID",
  .datetime_col = "DATETIME"
) {
  # The top-level object must be a list, not a single data frame.
  if (!is.list(.tv) || is.data.frame(.tv)) {
    cli::cli_abort("{.arg .tv} must be a named list of data frames")
  }

  # Both row-key arguments must identify one usable column name.
  key_arguments <- list(
    .subject_col = .subject_col,
    .datetime_col = .datetime_col
  )
  purrr::iwalk(
    key_arguments,
    function(value, argument) {
      if (
        !is.character(value) ||
          length(value) != 1 ||
          is.na(value) ||
          !nzchar(value)
      ) {
        cli::cli_abort(
          "{.arg {argument}} must be a single non-empty string"
        )
      }
    }
  )

  # The two parts of the row key must refer to different columns.
  if (.subject_col == .datetime_col) {
    cli::cli_abort(
      "{.arg .subject_col} and {.arg .datetime_col} must be different"
    )
  }

  # An empty list cannot produce a time-varying dataset.
  if (length(.tv) == 0) {
    cli::cli_abort("{.arg .tv} must contain at least one time-varying element")
  }

  # Stable, unique names identify the event type in each element.
  tv_names <- names(.tv)
  if (
    is.null(tv_names) ||
      anyNA(tv_names) ||
      any(!nzchar(tv_names)) ||
      anyDuplicated(tolower(tv_names))
  ) {
    cli::cli_abort(
      "{.arg .tv} must have unique, non-empty, case-insensitive names"
    )
  }

  # Inspect every element and report all structural problems in one error.
  issues <-
    purrr::imap(.tv, function(piece, piece_name) {
      label <- paste0("derived$tv$", piece_name)
      piece_issues <- character()

      # Every element must be a data frame before column checks are safe.
      if (!is.data.frame(piece)) {
        return(paste0(label, " must be a data frame"))
      }

      # Grouping can silently change later sorting, filling, and summaries.
      if (dplyr::is_grouped_df(piece)) {
        piece_issues <- c(
          piece_issues,
          paste0(label, " must be ungrouped")
        )
      }

      # Each event type must contribute at least one row.
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

      # List columns cannot be row-bound as ordinary model-data fields.
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

      # Every event needs both parts of the expected row key.
      required_columns <- c(.subject_col, .datetime_col)
      missing_columns <- setdiff(required_columns, names(piece))
      if (length(missing_columns) > 0) {
        piece_issues <- c(
          piece_issues,
          paste0(
            label,
            " must contain required column(s): ",
            paste(missing_columns, collapse = ", ")
          )
        )
      }

      # Check key values only when both columns are unambiguous and atomic;
      # earlier checks explain why unusable columns fail.
      usable_keys <-
        !duplicate_columns &&
        length(missing_columns) == 0 &&
        !any(purrr::map_lgl(piece[required_columns], is.list))

      if (usable_keys) {
        subject_values <- piece[[.subject_col]]
        datetime_values <- piece[[.datetime_col]]
        missing_subject <-
          is.na(subject_values) |
          !nzchar(trimws(as.character(subject_values)))
        missing_datetime <-
          is.na(datetime_values) |
          !nzchar(trimws(as.character(datetime_values)))

        # Missing or blank subjects cannot identify an event row.
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

        # Missing or blank datetimes cannot place an event chronologically.
        if (any(missing_datetime)) {
          piece_issues <- c(
            piece_issues,
            paste0(
              label,
              " has ",
              sum(missing_datetime),
              " missing or blank ",
              .datetime_col,
              " value(s)"
            )
          )
        }

        # USUBJID + DATETIME must identify one row within this element.
        # The same key may still occur in a different time-varying element.
        duplicate_keys <-
          piece %>%
          dplyr::ungroup() %>%
          dplyr::filter(!missing_subject, !missing_datetime) %>%
          dplyr::count(
            dplyr::across(dplyr::all_of(required_columns)),
            name = ".n"
          ) %>%
          dplyr::filter(.data$.n > 1)

        if (nrow(duplicate_keys) > 0) {
          duplicate_key_values <- paste0(
            as.character(duplicate_keys[[.subject_col]]),
            " @ ",
            as.character(duplicate_keys[[.datetime_col]])
          )
          piece_issues <- c(
            piece_issues,
            paste0(
              label,
              " has duplicated ",
              .subject_col,
              " + ",
              .datetime_col,
              " key(s): ",
              format_derived_issue_values(duplicate_key_values)
            )
          )
        }
      }

      piece_issues
    }) %>%
    purrr::flatten_chr()

  # Confirm shared columns have compatible types before the real bind_rows().
  bindable <- purrr::every(.tv, is.data.frame) &&
    purrr::every(.tv, ~ !anyDuplicated(names(.x)))
  if (bindable) {
    bind_error <- tryCatch(
      {
        dplyr::bind_rows(.tv)
        NULL
      },
      error = function(error) conditionMessage(error)
    )
    if (!is.null(bind_error)) {
      issues <- c(
        issues,
        paste0(
          "derived$tv elements cannot be combined with bind_rows(): ",
          bind_error
        )
      )
    }
  }

  # Return one readable error containing every issue found above.
  if (length(issues) > 0) {
    cli::cli_abort(c(
      "{.arg .tv} failed time-varying structure checks:",
      stats::setNames(issues, rep("x", length(issues)))
    ))
  }

  # Preserve the input so this assertion can be used in a pipeline.
  invisible(.tv)
}
