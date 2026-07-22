src_correction_columns <- function() {
  c(
    "domain",
    "USUBJID",
    "seq_variable",
    "seq_value",
    "variable",
    "original_value",
    "corrected_value",
    "reason"
  )
}

empty_src_corrections <- function() {
  tibble::tibble(
    domain = character(),
    USUBJID = character(),
    seq_variable = character(),
    seq_value = character(),
    variable = character(),
    original_value = character(),
    corrected_value = character(),
    reason = character()
  )
}

normalize_src_corrections <- function(.corrections) {
  if (is.null(.corrections)) {
    return(empty_src_corrections())
  }
  if (!is.data.frame(.corrections)) {
    cli::cli_abort("{.arg .corrections} must be a data frame or NULL")
  }
  if (nrow(.corrections) == 0 && ncol(.corrections) == 0) {
    return(empty_src_corrections())
  }

  required <- src_correction_columns()
  missing_columns <- setdiff(required, names(.corrections))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "{.arg .corrections} is missing required column{?s}: {.val {missing_columns}}"
    )
  }

  corrections <-
    .corrections %>%
    dplyr::transmute(
      dplyr::across(
        dplyr::all_of(required),
        as.character
      )
    ) %>%
    dplyr::mutate(
      seq_variable = dplyr::na_if(.data$seq_variable, ""),
      seq_value = dplyr::na_if(.data$seq_value, "")
    ) %>%
    dplyr::arrange(
      tolower(.data$domain),
      .data$USUBJID,
      tolower(dplyr::coalesce(.data$seq_variable, "")),
      dplyr::coalesce(.data$seq_value, ""),
      tolower(.data$variable)
    )

  required_nonempty <- c("domain", "USUBJID", "variable", "reason")
  invalid_nonempty <- purrr::map_lgl(
    required_nonempty,
    ~ any(is.na(corrections[[.x]]) | !nzchar(trimws(corrections[[.x]])))
  )
  if (any(invalid_nonempty)) {
    cli::cli_abort(
      "Correction column{?s} {.val {required_nonempty[invalid_nonempty]}} must not contain missing or empty values"
    )
  }

  incomplete_sequence <- xor(
    is.na(corrections$seq_variable),
    is.na(corrections$seq_value)
  )
  if (any(incomplete_sequence)) {
    cli::cli_abort(
      "{.field seq_variable} and {.field seq_value} must either both be populated or both be missing"
    )
  }

  duplicate_targets <-
    corrections %>%
    dplyr::transmute(
      domain = tolower(.data$domain),
      USUBJID = .data$USUBJID,
      seq_variable = tolower(.data$seq_variable),
      seq_value = .data$seq_value,
      variable = tolower(.data$variable)
    ) %>%
    duplicated()
  if (any(duplicate_targets)) {
    cli::cli_abort(
      "{.arg .corrections} contains duplicate corrections for the same source record and variable"
    )
  }

  corrections
}

src_value_as_character <- function(.value) {
  if (length(.value) != 1) {
    cli::cli_abort("Internal error: source correction values must be scalar")
  }
  if (is.na(.value)) {
    return(NA_character_)
  }
  as.character(.value)
}

coerce_src_correction <- function(.value, .column, .label) {
  if (length(.value) != 1) {
    cli::cli_abort(
      "Correction {.val {(.label)}} must provide one corrected value"
    )
  }
  if (is.na(.value)) {
    return(.column[NA_integer_])
  }

  abort_conversion <- function() {
    cli::cli_abort(
      "Correction {.val {(.label)}} value {.val {(.value)}} cannot be converted to source class {.val {paste(class(.column), collapse = ', ')}}"
    )
  }

  if (inherits(.column, "Date")) {
    value <- suppressWarnings(as.Date(.value))
    if (is.na(value)) abort_conversion()
    return(value)
  }

  if (inherits(.column, c("POSIXct", "POSIXlt"))) {
    timezone <- attr(.column, "tzone")
    if (is.null(timezone) || length(timezone) == 0) timezone <- "UTC"
    value <- suppressWarnings(as.POSIXct(.value, tz = timezone[[1]]))
    if (is.na(value)) abort_conversion()
    return(value)
  }

  if (is.factor(.column)) {
    if (!(.value %in% levels(.column))) abort_conversion()
    return(factor(.value, levels = levels(.column), ordered = is.ordered(.column)))
  }

  value <- switch(
    typeof(.column),
    character = .value,
    integer = suppressWarnings(readr::parse_integer(.value, na = character())),
    double = suppressWarnings(readr::parse_double(.value, na = character())),
    logical = suppressWarnings(readr::parse_logical(.value, na = character())),
    complex = suppressWarnings(as.complex(.value)),
    abort_conversion()
  )

  parse_failed <- is.na(value) ||
    (!is.null(attr(value, "problems")) && nrow(attr(value, "problems")) > 0)
  if (parse_failed) abort_conversion()

  value[[1]]
}

write_src_corrections_incomplete <- function(.report_file) {
  fs::dir_create(fs::path_dir(.report_file))
  readr::write_lines(
    c(
      "# Source Corrections",
      "",
      "The current source-read run has not completed."
    ),
    .report_file
  )
}

write_src_corrections_complete <- function(.corrections, .report_file) {
  lines <- if (nrow(.corrections) == 0) {
    c(
      "# Source Corrections",
      "",
      "No source corrections were applied."
    )
  } else {
    c(
      "# Source Corrections",
      "",
      knitr::kable(
        .corrections,
        format = "pipe",
        row.names = FALSE,
        na = ""
      )
    )
  }

  readr::write_lines(lines, .report_file)
}

print_src_correction <- function(
  .before,
  .after,
  .before_context,
  .after_context,
  .context_rows,
  .target_row,
  .correction,
  .index,
  .total,
  .print,
  .show_context
) {
  if (.print == "none") return(invisible(NULL))

  seq_key <- if (is.na(.correction$seq_variable)) {
    "no sequence variable"
  } else {
    paste0(.correction$seq_variable, " = ", .correction$seq_value)
  }

  cli::cli_h2("Source correction {(.index)} of {(.total)}")
  cli::cli_bullets(c(
    "i" = "Domain: {(.correction$domain)}",
    "i" = "Record: USUBJID = {(.correction$USUBJID)}, {seq_key}",
    "i" = "Variable: {(.correction$variable)}",
    "i" = "Reason: {(.correction$reason)}"
  ))

  comparison <- if (.print == "record") {
    dplyr::bind_rows(
      before = tibble::as_tibble(.before),
      after = tibble::as_tibble(.after),
      .id = "state"
    )
  } else {
    tibble::tibble(
      state = c("before", "after"),
      domain = .correction$domain,
      USUBJID = .correction$USUBJID,
      seq_variable = .correction$seq_variable,
      seq_value = .correction$seq_value,
      variable = .correction$variable,
      value = c(
        src_value_as_character(.before[[.correction$variable]]),
        src_value_as_character(.after[[.correction$variable]])
      )
    )
  }

  print(comparison, n = Inf, width = Inf)

  if (.show_context) {
    format_context <- function(.data) {
      context <-
        tibble::as_tibble(.data) %>%
        dplyr::mutate(
          source_row = .context_rows,
          target = .context_rows == .target_row
        ) %>%
        dplyr::relocate(dplyr::all_of(c("source_row", "target")))

      if (.print == "record") {
        return(context)
      }

      timing_variables <- names(.data)[
        grepl(
          "(DTC|DT|TM|VISIT|VISITNUM|CYCLE|DAY)$",
          names(.data),
          ignore.case = TRUE
        )
      ]
      sequence_variable <- .correction$seq_variable[
        !is.na(.correction$seq_variable)
      ]
      context_variables <- unique(c(
        "source_row",
        "target",
        "USUBJID",
        sequence_variable,
        timing_variables,
        .correction$variable
      ))

      dplyr::select(context, dplyr::any_of(context_variables))
    }

    cli::cli_h3("Source context before correction")
    print(format_context(.before_context), n = Inf, width = Inf)
    cli::cli_h3("Source context after correction")
    print(format_context(.after_context), n = Inf, width = Inf)
  }

  invisible(NULL)
}

#' Apply and document confirmed source-data corrections
#'
#' @description
#' Apply a declarative table of confirmed corrections to a source list, append
#' executable audit data as `corrections`, and regenerate a Markdown correction
#' report. Every correction is preflighted before any source value is changed.
#' Corrections are applied, printed, and logged in canonical source-key order.
#'
#' The incomplete-run notice is written before `.src_list` is evaluated. Pass
#' [read_src_dir()] directly as `.src_list` so a failed source read cannot leave
#' a stale successful report.
#'
#' @param .src_list A named list of source data frames, usually a direct call to
#'   [read_src_dir()].
#' @param .corrections A data frame with one row per corrected source record and
#'   columns `domain`, `USUBJID`, `seq_variable`, `seq_value`, `variable`,
#'   `original_value`, `corrected_value`, and `reason`. Values are represented
#'   as character data so corrections to mixed source types can be row-bound.
#'   `seq_variable` and `seq_value` normally hold the domain sequence key; use
#'   another single-record source key when the domain has no sequence variable.
#'   Use `NULL` or a zero-row data frame when no corrections are required.
#' @param .report_file Path to the Markdown correction report.
#' @param .print Console display mode. `"changed"` prints a focused before/after
#'   comparison plus focused context, `"record"` prints complete source rows,
#'   and `"none"` suppresses correction output.
#' @param .context Number of neighboring source rows to print on each side of
#'   the corrected row. Context is limited to the same `USUBJID` and retains
#'   source order. Focused output includes source identifiers, timing and visit
#'   variables, and the corrected variable. Set to `0` to suppress the context
#'   tables.
#'
#' @return The source list with corrections applied and a typed `corrections`
#'   tibble added.
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' report <- tempfile(fileext = ".md")
#'
#' corrections <- tibble::tribble(
#'   ~domain, ~USUBJID, ~seq_variable, ~seq_value, ~variable,
#'   ~original_value, ~corrected_value, ~reason,
#'   "dm", "CDISC01.100008", NA, NA, "AGE",
#'   "72", "73", "Confirmed against the source query"
#' )
#'
#' src <- apply_src_corrections(
#'   .src_list = read_src_dir(path, .file_types = "xpt", .read_domains = "dm"),
#'   .corrections = corrections,
#'   .report_file = report
#' )
#'
#' @export
apply_src_corrections <- function(
  .src_list,
  .corrections = NULL,
  .report_file,
  .print = c("changed", "record", "none"),
  .context = 2L
) {
  .print <- match.arg(.print)
  if (
    !is.numeric(.context) ||
      length(.context) != 1 ||
      is.na(.context) ||
      !is.finite(.context) ||
      .context < 0 ||
      .context != floor(.context) ||
      .context > .Machine$integer.max
  ) {
    cli::cli_abort("{.arg .context} must be one non-negative whole number")
  }
  .context <- as.integer(.context)
  if (
    missing(.report_file) ||
      !is.character(.report_file) ||
      length(.report_file) != 1 ||
      is.na(.report_file) ||
      !nzchar(.report_file)
  ) {
    cli::cli_abort("{.arg .report_file} must be a single non-empty path")
  }

  write_src_corrections_incomplete(.report_file)
  corrections <- normalize_src_corrections(.corrections)

  src_list <- .src_list
  if (!is.list(src_list) || is.data.frame(src_list)) {
    cli::cli_abort("{.arg .src_list} must be a named list of source data frames")
  }
  if (is.null(names(src_list)) || any(!nzchar(names(src_list)))) {
    cli::cli_abort("{.arg .src_list} must have non-empty names")
  }
  if (anyDuplicated(names(src_list))) {
    cli::cli_abort("{.arg .src_list} must have unique names")
  }
  metadata_elements <- startsWith(names(src_list), "mrgda_")
  non_data_frames <- names(src_list)[
    !metadata_elements & !purrr::map_lgl(src_list, is.data.frame)
  ]
  if (length(non_data_frames) > 0) {
    cli::cli_abort(
      "{.arg .src_list} element{?s} {.val {non_data_frames}} must be data frames"
    )
  }
  existing_corrections <- src_list[["corrections"]]
  if (!is.null(existing_corrections)) {
    if (nrow(existing_corrections) > 0) {
      cli::cli_abort("{.arg .src_list} already contains applied corrections")
    }
  }
  src_list$corrections <- empty_src_corrections()

  source_domains <- names(src_list)[
    !startsWith(names(src_list), "mrgda_") &
      names(src_list) != "corrections"
  ]

  prepared <- purrr::map(seq_len(nrow(corrections)), function(i) {
    correction <- corrections[i, , drop = FALSE]
    label <- paste0(
      correction$domain,
      "/",
      correction$USUBJID,
      "/",
      if (is.na(correction$seq_variable)) {
        "no-sequence"
      } else {
        paste0(correction$seq_variable, "=", correction$seq_value)
      },
      "/",
      correction$variable
    )

    domain_match <- source_domains[
      tolower(source_domains) == tolower(correction$domain)
    ]
    if (length(domain_match) != 1) {
      cli::cli_abort(
        "Correction {.val {label}} expected exactly one source domain; found {length(domain_match)}"
      )
    }
    domain <- domain_match[[1]]
    data <- src_list[[domain]]

    required_variables <- c("USUBJID", correction$variable)
    if (!is.na(correction$seq_variable)) {
      required_variables <- c(required_variables, correction$seq_variable)
    }
    missing_variables <- setdiff(required_variables, names(data))
    if (length(missing_variables) > 0) {
      cli::cli_abort(
        "Correction {.val {label}} source domain is missing variable{?s}: {.val {missing_variables}}"
      )
    }

    record_match <-
      !is.na(data$USUBJID) &
      as.character(data$USUBJID) == correction$USUBJID
    if (!is.na(correction$seq_variable)) {
      sequence_values <- data[[correction$seq_variable]]
      record_match <-
        record_match &
        !is.na(sequence_values) &
        as.character(sequence_values) == correction$seq_value
    }
    record_index <- which(record_match)
    if (length(record_index) != 1) {
      cli::cli_abort(
        "Correction {.val {label}} expected exactly one source record; found {length(record_index)}"
      )
    }

    subject_rows <- which(
      !is.na(data$USUBJID) &
        as.character(data$USUBJID) == correction$USUBJID
    )
    subject_position <- match(record_index, subject_rows)
    context_start <- max(1L, subject_position - .context)
    context_end <- min(length(subject_rows), subject_position + .context)
    context_rows <- subject_rows[seq.int(context_start, context_end)]

    actual_original <- src_value_as_character(
      data[[correction$variable]][record_index]
    )
    original_matches <- if (is.na(correction$original_value)) {
      is.na(actual_original)
    } else {
      identical(actual_original, correction$original_value)
    }
    if (!original_matches) {
      cli::cli_abort(
        "Correction {.val {label}} expected original value {.val {correction$original_value}} but found {.val {actual_original}}"
      )
    }

    list(
      index = i,
      domain = domain,
      row = record_index,
      context_rows = context_rows,
      variable = correction$variable,
      corrected_value = coerce_src_correction(
        correction$corrected_value,
        data[[correction$variable]],
        label
      )
    )
  })

  src_list <-
    purrr::reduce(
      prepared,
      .init = src_list,
      .f = function(src_list, item) {
        i <- item$index
        correction <- corrections[i, , drop = FALSE]
        data <- src_list[[item$domain]]
        before <- data[item$row, , drop = FALSE]
        before_context <- data[item$context_rows, , drop = FALSE]
        data[[item$variable]][item$row] <- item$corrected_value
        after <- data[item$row, , drop = FALSE]
        after_context <- data[item$context_rows, , drop = FALSE]

        actual_corrected <- src_value_as_character(after[[item$variable]])
        intended_corrected <- src_value_as_character(item$corrected_value)
        if (!identical(actual_corrected, intended_corrected)) {
          cli::cli_abort(
            "Internal error: source correction assignment did not produce the intended value"
          )
        }

        src_list[[item$domain]] <- data

        correction$domain <- item$domain
        correction$original_value <- src_value_as_character(
          before[[item$variable]]
        )
        correction$corrected_value <- src_value_as_character(
          after[[item$variable]]
        )
        src_list$corrections <- dplyr::bind_rows(
          src_list$corrections,
          correction
        )

        print_src_correction(
          .before = before,
          .after = after,
          .before_context = before_context,
          .after_context = after_context,
          .context_rows = item$context_rows,
          .target_row = item$row,
          .correction = correction,
          .index = i,
          .total = length(prepared),
          .print = .print,
          .show_context = .context > 0
        )

        src_list
      }
    )

  write_src_corrections_complete(src_list$corrections, .report_file)

  if (.print != "none") {
    if (nrow(src_list$corrections) == 0) {
      cli::cli_alert_info("No source corrections were applied.")
    } else {
      cli::cli_alert_success(
        "Applied {nrow(src_list$corrections)} confirmed source correction{?s}."
      )
    }
    cli::cli_alert_success(
      "Source correction report: {.path {(.report_file)}}"
    )
  }

  src_list
}
