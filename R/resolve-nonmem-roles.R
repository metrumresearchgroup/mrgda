#' Default column names to try for each NONMEM data field
#'
#' Each item in this list is a NONMEM data field that mrgda knows how to find.
#' The values are the column names that will be tried for that field.
#'
#' For example:
#'
#' dv = "DV"
#'
#' means:
#'
#' For the `dv` field, look for a dataset column named "DV".
#'
#' These defaults are intentionally conservative. Project-specific names and
#' analysis-specific names should be supplied through `.role_overrides` rather
#' than auto-detected.
#'
#' @noRd
default_nonmem_column_names_by_role <- list(
  study_id = c("STUDYID", "STUDY"),
  id = "ID",
  subject_label = c("SUBJID", "USUBJID"),
  time = c("TIME", "TAFD"),
  tad = c("TAD", "TSD"),
  dv = "DV",
  evid = "EVID",
  mdv = "MDV",
  amt = "AMT",
  rate = "RATE",
  dur = "DUR",
  cmt = "CMT",
  dvid = "DVID",
  blq = c("BLQ", "BLOQ", "BQL"),
  dose_group = c("DOSE", "DOSEGRP"),
  day = "DAY",
  datetime = c("DATETIME", "DATE"),
  comment = c("C", "EXCL", "IGNORE", "DROP"),
  occasion = "OCC"
)

#' Resolve NONMEM data fields
#'
#' This function looks at the column names in a NONMEM-style dataset and tries
#' to decide which column should be used for each common NONMEM data field.
#'
#' For example, if the dataset has a column named "DV", this function will mark
#' that column as the `dv` field.
#'
#' The function checks for columns in this order:
#'
#' 1. Use a user-provided override from `.role_overrides`, if one was supplied.
#' 2. Look for one of the default NONMEM-style column names.
#' 3. If no match is found, leave the field unresolved.
#'
#' @details
#' By default, this function looks for the following columns:
#'
#' \describe{
#'   \item{study_id}{Study identifier. Looks for "STUDYID", then "STUDY".}
#'   \item{id}{Subject ID. Looks for "ID".}
#'   \item{subject_label}{Subject label used outside of NONMEM. Looks for "SUBJID", then "USUBJID".}
#'   \item{time}{Time after first dose. Looks for "TIME", then "TAFD".}
#'   \item{tad}{Time after most recent dose. Looks for "TAD", then "TSD".}
#'   \item{dv}{Observed value or dependent variable. Looks for "DV".}
#'   \item{evid}{Event type, such as observation or dose. Looks for "EVID".}
#'   \item{mdv}{Flag for whether the dependent variable is missing or ignored. Looks for "MDV".}
#'   \item{amt}{Dose amount. Looks for "AMT".}
#'   \item{rate}{Dose infusion rate. Looks for "RATE".}
#'   \item{dur}{Dose infusion duration. Looks for "DUR".}
#'   \item{cmt}{Compartment number or compartment label. Looks for "CMT".}
#'   \item{dvid}{Observed value type, such as parent or metabolite. Looks for "DVID".}
#'   \item{blq}{Flag for values below the quantification limit. Looks for "BLQ", then "BLOQ", then "BQL".}
#'   \item{dose_group}{Dose group or planned dose level. Looks for "DOSE", then "DOSEGRP".}
#'   \item{day}{Study day or analysis day. Looks for "DAY".}
#'   \item{datetime}{Date or date-time value for the record. Looks for "DATETIME", then "DATE".}
#'   \item{comment}{Column used to flag, exclude, ignore, or drop records. Looks for "C", then "EXCL", then "IGNORE", then "DROP".}
#'   \item{occasion}{Dosing occasion. Looks for "OCC".}
#' }
#'
#' @param .data A data frame containing the NONMEM-style dataset.
#' @param .role_overrides A named list of user-supplied column choices. Each
#'   name should be one of the fields listed in Details. Each value may be
#'   `NULL` or omitted for auto-detect, one column name, or `FALSE` to turn off
#'   that field.
#'
#' @return A data frame with one row per NONMEM data field. The columns are:
#' \describe{
#'   \item{role}{The NONMEM data field being resolved.}
#'   \item{column}{The dataset column found for that field, or `NA` if none was found.}
#'   \item{source}{How the column was found.}
#'   \item{status}{Whether the lookup worked.}
#' }
#'
#' @examples
#' nonmem_data <- data.frame(
#'   STUDYID = "ABC123",
#'   ID = 1,
#'   TIME = 0,
#'   DV = 1.2,
#'   EVID = 0
#' )
#'
#' resolve_nonmem_roles(nonmem_data)
#'
#' resolve_nonmem_roles(
#'   nonmem_data,
#'   .role_overrides = list(dv = "DV")
#' )
#'
#' @export
resolve_nonmem_roles <- function(.data, .role_overrides = list()) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data frame.", call. = FALSE)
  }

  if (is.null(.role_overrides)) {
    .role_overrides <- list()
  }

  if (!is.list(.role_overrides)) {
    stop("`.role_overrides` must be a named list.", call. = FALSE)
  }

  dataset_column_names <- names(.data)

  # Build one row for each NONMEM field.
  #
  # Each row answers four plain questions:
  #
  # What field were we trying to find?
  # What dataset column did we find?
  # How did we find it?
  # Did the lookup work?
  default_nonmem_column_names_by_role %>%
    purrr::imap_dfr(
      function(default_column_names_for_this_field, field_name) {
        resolve_one_nonmem_field_to_one_column(
          field_name = field_name,
          dataset_column_names = dataset_column_names,
          user_column_choice = .role_overrides[[field_name]],
          default_column_names_for_this_field = default_column_names_for_this_field
        )
      }
    )
}

#' Resolve one NONMEM field to one dataset column
#'
#' This helper handles one field at a time.
#'
#' For example, the main function may ask:
#'
#' Which dataset column should be used for `dv`?
#'
#' This helper then checks:
#'
#' 1. Did the user turn off this field with `FALSE`?
#' 2. Did the user provide a column name for this field?
#' 3. Does the dataset contain one of the default column names for this field?
#' 4. If none of those worked, mark the field as skipped.
#'
#' @param field_name The NONMEM field being resolved, such as "dv" or "time".
#' @param dataset_column_names The column names from the dataset.
#' @param user_column_choice The user-provided value for this field, if any.
#' @param default_column_names_for_this_field The default column names to try
#'   for this field.
#'
#' @return One row for the output data frame.
#'
#' @noRd
resolve_one_nonmem_field_to_one_column <- function(
  field_name,
  dataset_column_names,
  user_column_choice,
  default_column_names_for_this_field
) {
  # FALSE means the user intentionally turned off this field.
  #
  # Example:
  #
  # .role_overrides = list(mdv = FALSE)
  #
  # In that case, do not try to find an MDV column, even if one exists.
  if (isFALSE(user_column_choice)) {
    return(nonmem_field_result_row(
      role = field_name,
      column = NA_character_,
      source = "override",
      status = "disabled"
    ))
  }

  # Clean up the user-provided column choice before using it.
  #
  # This removes blanks, removes missing values, and removes repeats.
  cleaned_user_column_choices <- clean_column_names(user_column_choice)

  # In this first version, each NONMEM field can only point to one column.
  #
  # Example:
  #
  # .role_overrides = list(dv = "CONC")
  #
  # is allowed.
  #
  # .role_overrides = list(dv = c("CONC", "LIDV"))
  #
  # is not allowed.
  if (length(cleaned_user_column_choices) > 1) {
    stop(
      "Role override for `",
      field_name,
      "` must be one column name.",
      call. = FALSE
    )
  }

  # If the user supplied a column name, use that instead of guessing.
  #
  # Example:
  #
  # .role_overrides = list(dv = "CONC")
  #
  # means:
  #
  # Use "CONC" for the `dv` field.
  if (length(cleaned_user_column_choices)) {
    user_column_name <- cleaned_user_column_choices[[1]]

    user_column_was_found <- user_column_name %in% dataset_column_names

    return(nonmem_field_result_row(
      role = field_name,
      column = if (user_column_was_found) {
        user_column_name
      } else {
        NA_character_
      },
      source = "override",
      status = if (user_column_was_found) {
        "OK"
      } else {
        "missing column"
      }
    ))
  }

  # If the user did not supply a column name, try the default NONMEM-style
  # column names for this field.
  found_column <- find_first_matching_nonmem_column(
    possible_column_names = default_column_names_for_this_field,
    dataset_column_names = dataset_column_names
  )

  # If a default column was found, return it and mark it as OK.
  if (length(found_column)) {
    return(nonmem_field_result_row(
      role = field_name,
      column = found_column[["column"]],
      source = found_column[["source"]],
      status = "OK"
    ))
  }

  # If nothing matched, keep this field in the result but mark it as skipped.
  nonmem_field_result_row(
    role = field_name,
    column = NA_character_,
    source = "not found",
    status = "skipped"
  )
}

#' Find the first matching column for a NONMEM field
#'
#' This helper compares two sets of names:
#'
#' - the column names we are willing to accept for a field
#' - the column names that actually exist in the dataset
#'
#' It returns the first dataset column that matches one of the accepted names.
#'
#' @param possible_column_names Column names that could represent this field.
#' @param dataset_column_names Column names that exist in the dataset.
#'
#' @return A named character vector with `column` and `source`, or `NULL` if
#'   no column was found.
#'
#' @noRd
find_first_matching_nonmem_column <- function(
  possible_column_names,
  dataset_column_names
) {
  cleaned_possible_column_names <- clean_column_names(possible_column_names)

  # Try the accepted column names in order.
  #
  # Example:
  #
  # possible_column_names = c("STUDYID", "STUDY")
  #
  # This checks for "STUDYID" first, then "STUDY".
  for (possible_column_name in cleaned_possible_column_names) {
    if (possible_column_name %in% dataset_column_names) {
      return(c(
        column = possible_column_name,
        source = "default name"
      ))
    }
  }

  # If none of the accepted names were found, return NULL.
  NULL
}

#' Clean column names
#'
#' This helper takes possible column names and turns them into a clean character
#' vector that is safe to use for matching.
#'
#' It does three things:
#'
#' 1. Converts the input to character values.
#' 2. Removes missing values and blank strings.
#' 3. Removes duplicate column names.
#'
#' This is used for both:
#'
#' - the default NONMEM column names, such as "DV", "TIME", and "EVID"
#' - user-provided overrides from `.role_overrides`
#'
#' @param column_names Column names to clean.
#'
#' @return A clean character vector of column names.
#'
#' @noRd
clean_column_names <- function(column_names) {
  # Convert the input to character values so the rest of the function can
  # treat everything the same way.
  #
  # Examples:
  #
  # NULL becomes character(0)
  # "DV" stays "DV"
  # c("DV", "TIME") stays c("DV", "TIME")
  column_names_as_text <- column_names %>%
    as.character()

  # Keep only real column names.
  #
  # nzchar() checks whether each string has at least one character.
  # keepNA = FALSE means missing values are treated like empty strings and
  # removed here too.
  #
  # Examples removed here:
  #
  # NA
  # ""
  non_empty_column_names <- column_names_as_text %>%
    .[nzchar(., keepNA = FALSE)]

  # Remove repeated names so matching only has to consider each possible
  # column name once.
  #
  # Example:
  #
  # c("DV", "DV", "TIME") becomes c("DV", "TIME")
  non_empty_column_names %>%
    unique()
}

#' Build one row of NONMEM field information
#'
#' This helper creates one row for the output data frame.
#'
#' The row records:
#'
#' - the NONMEM field we tried to find
#' - the dataset column we found
#' - how the column was found
#' - whether the lookup worked
#'
#' @param role The NONMEM field name, such as "dv", "time", or "id".
#' @param column The dataset column found for that field, or `NA` if none was
#'   found.
#' @param source How the column was found.
#' @param status Whether the lookup worked.
#'
#' @return A one-row tibble.
#'
#' @noRd
nonmem_field_result_row <- function(role, column, source, status) {
  tibble::tibble(
    role = role,
    column = column,
    source = source,
    status = status
  )
}
