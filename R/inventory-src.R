#' Inventory a source data list
#'
#' @description
#' Create a compact, deterministic inventory of the clinical source datasets
#' returned by [read_src_dir()]. The inventory summarizes domain dimensions,
#' identifiers, likely sequence variables, date/time and result variables, key
#' integrity, variable classes, labels, missingness, and cardinality.
#'
#' `inventory_src()` reports source structure; it does not validate scientific
#' plausibility or modify source data.
#'
#' @param .src_list A named list of source data frames, typically returned by
#'   [read_src_dir()]. Elements whose names start with `mrgda_` are treated as
#'   package metadata and excluded from the domain inventory. The executable
#'   `corrections` audit returned by [apply_src_corrections()] is also excluded.
#' @param .subject_col A single character string naming the subject identifier.
#'   Defaults to `"USUBJID"`.
#'
#' @return An object of class `mrgda_src_inventory`. This is a list containing:
#'   \describe{
#'   \item{domains}{One row per source domain with dimensions, inferred source
#'   variables, and key-integrity counts.}
#'   \item{variables}{One row per source variable with its label, class, inferred
#'   role, missing count, and distinct-value count.}
#'   }
#'
#' @examples
#' path <- system.file("example-sdtm", package = "mrgda")
#' src <- read_src_dir(path, .file_types = "xpt")
#' inventory_src(src)
#'
#' @export
inventory_src <- function(.src_list, .subject_col = "USUBJID") {
  if (!is.list(.src_list) || is.data.frame(.src_list)) {
    cli::cli_abort("{.arg .src_list} must be a named list of data frames")
  }
  if (
    !is.character(.subject_col) ||
      length(.subject_col) != 1 ||
      is.na(.subject_col) ||
      !nzchar(.subject_col)
  ) {
    cli::cli_abort("{.arg .subject_col} must be a single non-empty string")
  }

  src_names <- names(.src_list)
  if (
    is.null(src_names) ||
      anyNA(src_names) ||
      any(!nzchar(src_names)) ||
      anyDuplicated(src_names)
  ) {
    cli::cli_abort("{.arg .src_list} must have unique, non-empty names")
  }

  domain_names <-
    tibble::tibble(
      domain = src_names[
        !startsWith(src_names, "mrgda_") &
          src_names != "corrections" &
          purrr::map_lgl(.src_list, is.data.frame)
      ]
    ) %>%
    dplyr::arrange(.data$domain) %>%
    dplyr::pull(.data$domain)
  if (length(domain_names) == 0) {
    cli::cli_abort("{.arg .src_list} does not contain any source data frames")
  }

  labels <- .src_list$mrgda_labels
  if (
    !is.data.frame(labels) ||
      !all(c("DOMAIN", "COLUMN_NAME", "COLUMN_LABEL") %in% names(labels))
  ) {
    labels <- NULL
  }

  collapse_variables <- function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    }
    paste(x, collapse = ", ")
  }

  inventory_rows <- purrr::map(domain_names, function(domain) {
    data <- .src_list[[domain]]
    data_names <- names(data)

    subject_variable <- data_names[tolower(data_names) == tolower(.subject_col)]
    if (length(subject_variable) == 0) {
      subject_variable <- NA_character_
    } else {
      subject_variable <- subject_variable[[1]]
    }

    sequence_candidates <- data_names[grepl("SEQ$", data_names, ignore.case = TRUE)]
    conventional_sequence <- paste0(toupper(domain), "SEQ")
    conventional_match <- sequence_candidates[
      toupper(sequence_candidates) == conventional_sequence
    ]
    sequence_variable <- if (length(conventional_match) > 0) {
      conventional_match[[1]]
    } else if (length(sequence_candidates) == 1) {
      sequence_candidates[[1]]
    } else {
      NA_character_
    }

    datetime_variables <- data_names[
      grepl("(DTC|DT|TM)$", data_names, ignore.case = TRUE) |
        purrr::map_lgl(
          data,
          ~ inherits(.x, c("Date", "POSIXct", "POSIXlt"))
        )
    ]
    result_variables <- data_names[
      grepl("(ORRES|STRESC|STRESN)$", data_names, ignore.case = TRUE)
    ]

    exact_duplicate_rows <- sum(duplicated(data))
    n_subjects <- NA_integer_
    missing_subjects <- NA_integer_
    missing_sequences <- NA_integer_
    excess_key_rows <- NA_integer_
    key_unique <- NA

    if (!is.na(subject_variable)) {
      subject_values <- data[[subject_variable]]
      n_subjects <- dplyr::n_distinct(subject_values, na.rm = TRUE)
      missing_subjects <- sum(is.na(subject_values))
    }

    if (!is.na(sequence_variable)) {
      missing_sequences <- sum(is.na(data[[sequence_variable]]))
    }

    if (!is.na(subject_variable) && !is.na(sequence_variable)) {
      key_data <- data[c(subject_variable, sequence_variable)]
      complete_key <- stats::complete.cases(key_data)
      excess_key_rows <- sum(duplicated(key_data[complete_key, , drop = FALSE]))
      key_unique <-
        excess_key_rows == 0 &&
        missing_subjects == 0 &&
        missing_sequences == 0
    }

    domain_row <- tibble::tibble(
      DOMAIN = domain,
      N_ROWS = nrow(data),
      N_COLUMNS = ncol(data),
      N_SUBJECTS = n_subjects,
      SUBJECT_VARIABLE = subject_variable,
      SEQUENCE_VARIABLE = sequence_variable,
      DATETIME_VARIABLES = collapse_variables(datetime_variables),
      RESULT_VARIABLES = collapse_variables(result_variables),
      N_MISSING_SUBJECT = missing_subjects,
      N_MISSING_SEQUENCE = missing_sequences,
      N_EXCESS_KEY_ROWS = excess_key_rows,
      N_EXACT_DUPLICATE_ROWS = exact_duplicate_rows,
      KEY_UNIQUE = key_unique
    )

    variable_row <- purrr::map_dfr(data_names, function(variable) {
      values <- data[[variable]]
      label <- attr(values, "label")

      if (is.null(label) && !is.null(labels)) {
        label_match <- labels[
          tolower(labels$DOMAIN) == tolower(domain) &
            labels$COLUMN_NAME == variable,
          "COLUMN_LABEL",
          drop = TRUE
        ]
        if (length(label_match) > 0) {
          label <- label_match[[1]]
        }
      }
      if (is.null(label) || length(label) == 0 || is.na(label[[1]])) {
        label <- NA_character_
      } else {
        label <- label[[1]]
      }

      role <- if (identical(variable, subject_variable)) {
        "subject"
      } else if (identical(variable, sequence_variable)) {
        "sequence"
      } else if (variable %in% datetime_variables) {
        "datetime"
      } else if (variable %in% result_variables) {
        "result"
      } else {
        "other"
      }

      tibble::tibble(
        DOMAIN = domain,
        VARIABLE = variable,
        LABEL = as.character(label),
        CLASS = paste(class(values), collapse = ", "),
        ROLE = role,
        N_MISSING = sum(is.na(values)),
        N_DISTINCT = dplyr::n_distinct(values, na.rm = TRUE)
      )
    })

    list(domains = domain_row, variables = variable_row)
  })

  structure(
    list(
      domains = purrr::map_dfr(inventory_rows, "domains"),
      variables = purrr::map_dfr(inventory_rows, "variables")
    ),
    class = "mrgda_src_inventory"
  )
}

#' @export
print.mrgda_src_inventory <- function(x, ...) {
  n_domains <- nrow(x$domains)
  n_variables <- nrow(x$variables)
  n_rows <- sum(x$domains$N_ROWS)
  n_key_issues <- sum(
    x$domains$N_EXCESS_KEY_ROWS,
    na.rm = TRUE
  ) + sum(
    x$domains$N_MISSING_SUBJECT,
    na.rm = TRUE
  ) + sum(
    x$domains$N_MISSING_SEQUENCE,
    na.rm = TRUE
  )

  cli::cli_h1("Source inventory")
  cli::cli_bullets(c(
    "*" = "{n_domains} domain{?s}",
    "*" = "{n_rows} source row{?s}",
    "*" = "{n_variables} variable{?s}",
    "*" = "{n_key_issues} missing or excess inferred-key row{?s}"
  ))

  invisible(x)
}
