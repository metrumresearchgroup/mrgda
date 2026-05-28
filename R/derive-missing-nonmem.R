# Missing NONMEM derivation helpers.

#' Derive missing NONMEM time after dose
#'
#' Adds a TAD column when a NONMEM-style data set has ID, TIME, and EVID but no
#' resolved TAD column.
#'
#' @param .data A data frame containing the NONMEM-style dataset.
#' @param .role_overrides A list of named user-supplied column choices passed to
#'   [resolve_nonmem_roles()].
#'
#' @return `.data`, unchanged when TAD is already resolved or disabled; otherwise
#'   `.data` with a derived TAD column.
#'
#' @noRd
derive_missing_nonmem_tad <- function(.data, .role_overrides = list()) {
  # Resolve column roles up front so derived values use the same column choices
  # as the rest of the NONMEM helper layer.
  resolved_roles <- resolve_nonmem_roles(
    .data = .data,
    .role_overrides = .role_overrides
  )

  # If TAD is already present, leave the dataset alone.
  #
  # If the user explicitly disabled TAD with:
  #
  # .role_overrides = list(tad = FALSE)
  #
  # also leave the dataset alone.
  if (nonmem_role_is_resolved_or_disabled(resolved_roles, "tad")) {
    return(.data)
  }

  # TAD can only be derived when we know:
  #
  # id: Which subject does each row belong to?
  # time: What is the row time?
  # evid: Which rows are dose rows?
  require_nonmem_roles_for_derivation(
    resolved_roles = resolved_roles,
    required_roles = c("id", "time", "evid"),
    derived_role_label = "TAD"
  )

  role_columns <- nonmem_time_derivation_role_columns(resolved_roles)

  # Use "TAD" by default.
  #
  # If the user supplied a TAD override to a missing column name, derive into
  # that column name instead.
  tad_column <- derived_nonmem_role_column_name(
    role_overrides = .role_overrides,
    role = "tad",
    default_column = "TAD"
  )

  # Build the shared dose-interval information once.
  #
  # This keeps TAD and OCC using the same subject ordering, same-time dose
  # handling, cumulative dose count, and most recent dose time.
  prepared_data <- prepare_nonmem_dose_interval_data(
    .data = .data,
    role_columns = role_columns,
    protected_column_names = tad_column
  )

  temporary_columns <- prepared_data$temporary_columns
  time_column <- temporary_columns[["mrgda_time"]]
  last_dose_time_column <- temporary_columns[["mrgda_last_dose_time"]]

  # TAD is the current row time minus the most recent dose time.
  #
  # Rows before the first dose have no most recent dose, so they get NA.
  prepared_data$data %>%
    dplyr::mutate(
      "{tad_column}" := ifelse(
        is.infinite(.data[[last_dose_time_column]]),
        NA_real_,
        .data[[time_column]] - .data[[last_dose_time_column]]
      )
    ) %>%
    restore_nonmem_original_order(temporary_columns)
}

#' Derive missing NONMEM occasion
#'
#' Adds an OCC column when a NONMEM-style data set has ID, TIME, and EVID but no
#' resolved OCC column.
#'
#' @inheritParams derive_missing_nonmem_tad
#'
#' @return `.data`, unchanged when OCC is already resolved or disabled; otherwise
#'   `.data` with a derived OCC column.
#'
#' @noRd
derive_missing_nonmem_occ <- function(.data, .role_overrides = list()) {
  # Resolve column roles up front so OCC uses the same role lookup as TAD and
  # the public resolver.
  resolved_roles <- resolve_nonmem_roles(
    .data = .data,
    .role_overrides = .role_overrides
  )

  # If OCC is already present, leave the dataset alone.
  #
  # If the user explicitly disabled occasion with:
  #
  # .role_overrides = list(occasion = FALSE)
  #
  # also leave the dataset alone.
  if (nonmem_role_is_resolved_or_disabled(resolved_roles, "occasion")) {
    return(.data)
  }

  # OCC can only be derived when we know:
  #
  # id: Which subject does each row belong to?
  # time: What is the row time?
  # evid: Which rows are dose rows and observation rows?
  require_nonmem_roles_for_derivation(
    resolved_roles = resolved_roles,
    required_roles = c("id", "time", "evid"),
    derived_role_label = "OCC"
  )

  role_columns <- nonmem_time_derivation_role_columns(resolved_roles)

  # Use "OCC" by default.
  #
  # If the user supplied an occasion override to a missing column name, derive
  # into that column name instead.
  occ_column <- derived_nonmem_role_column_name(
    role_overrides = .role_overrides,
    role = "occasion",
    default_column = "OCC"
  )

  # Build the shared dose-interval information once.
  #
  # This gives OCC the same cumulative dose count used by TAD, while preserving
  # the original row order for the final result.
  prepared_data <- prepare_nonmem_dose_interval_data(
    .data = .data,
    role_columns = role_columns,
    protected_column_names = occ_column
  )

  # OCC needs a few extra working columns on top of the shared dose-interval
  # columns. Keep their names collision-safe so user columns are never removed
  # by mistake when temporary columns are dropped.
  occ_temporary_columns <- unique_temporary_column_names(
    existing_column_names = c(names(prepared_data$data), occ_column),
    requested_column_names = c(
      "mrgda_is_pk_observation",
      "mrgda_dose_had_pk",
      "mrgda_new_dose"
    )
  )

  temporary_columns <- c(
    prepared_data$temporary_columns,
    occ_temporary_columns
  )

  original_id_column <- role_columns[["id"]]
  original_evid_column <- role_columns[["evid"]]
  n_doses_column <- temporary_columns[["mrgda_n_doses"]]
  is_pk_observation_column <- temporary_columns[["mrgda_is_pk_observation"]]
  dose_had_pk_column <- temporary_columns[["mrgda_dose_had_pk"]]
  new_dose_column <- temporary_columns[["mrgda_new_dose"]]

  # First, identify PK observations.
  #
  # Then, within each subject and dose interval, mark whether that dose interval
  # had at least one usable PK observation.
  #
  # Finally, count a new occasion each time we enter a new dose interval that
  # had usable PK. Rows before the first dose stay at OCC = 0.
  prepared_data$data %>%
    add_nonmem_pk_observation_column(
      evid_column = original_evid_column,
      blq_column = role_columns[["blq"]],
      is_pk_observation_column = is_pk_observation_column
    ) %>%
    dplyr::group_by(
      .data[[original_id_column]],
      .data[[n_doses_column]]
    ) %>%
    dplyr::mutate(
      "{dose_had_pk_column}" := any(
        .data[[is_pk_observation_column]],
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data[[original_id_column]]) %>%
    dplyr::mutate(
      "{new_dose_column}" := .data[[n_doses_column]] !=
        dplyr::lag(
          .data[[n_doses_column]],
          default = 0L
        ),
      "{occ_column}" := as.integer(
        cumsum(.data[[new_dose_column]] & .data[[dose_had_pk_column]]) *
          (.data[[n_doses_column]] != 0L)
      )
    ) %>%
    dplyr::ungroup() %>%
    restore_nonmem_original_order(temporary_columns)
}

#' @noRd
prepare_nonmem_dose_interval_data <- function(
  .data,
  role_columns,
  protected_column_names = character()
) {
  # The helper adds temporary columns and removes them before returning.
  #
  # Use generated names so an input data set with columns like
  # "mrgda_original_row" is still handled safely.
  temporary_columns <- unique_temporary_column_names(
    existing_column_names = c(names(.data), protected_column_names),
    requested_column_names = c(
      "mrgda_original_row",
      "mrgda_dose_sort",
      "mrgda_time",
      "mrgda_is_dose",
      "mrgda_n_doses",
      "mrgda_last_dose_time"
    )
  )

  original_id_column <- role_columns[["id"]]
  original_time_column <- role_columns[["time"]]
  original_evid_column <- role_columns[["evid"]]
  original_row_column <- temporary_columns[["mrgda_original_row"]]
  dose_sort_column <- temporary_columns[["mrgda_dose_sort"]]
  time_column <- temporary_columns[["mrgda_time"]]
  is_dose_column <- temporary_columns[["mrgda_is_dose"]]
  n_doses_column <- temporary_columns[["mrgda_n_doses"]]
  last_dose_time_column <- temporary_columns[["mrgda_last_dose_time"]]

  # Add the shared working columns.
  #
  # original_row: lets callers get rows back in the same order they came in.
  # time: a numeric version of the resolved TIME column.
  # is_dose: TRUE when EVID is 1 or 4.
  # dose_sort: puts dose rows before observations when they share a TIME.
  data <-
    .data %>%
    dplyr::mutate(
      "{original_row_column}" := dplyr::row_number(),
      "{time_column}" := as.numeric(.data[[original_time_column]]),
      "{is_dose_column}" := !is.na(.data[[original_evid_column]]) &
        .data[[original_evid_column]] %in% c(1, 4),
      "{dose_sort_column}" := ifelse(.data[[is_dose_column]], 0L, 1L)
    ) %>%
    dplyr::arrange(
      .data[[original_id_column]],
      .data[[time_column]],
      .data[[dose_sort_column]],
      .data[[original_row_column]]
    ) %>%
    # Work subject by subject so each subject gets an independent dose count
    # and most recent dose time.
    dplyr::group_by(.data[[original_id_column]]) %>%
    dplyr::mutate(
      # N_DOSES is the cumulative number of dose rows seen so far.
      "{n_doses_column}" := cumsum(.data[[is_dose_column]]),
      # The most recent dose time is carried forward after each dose.
      #
      # Before the first dose, this stays -Inf so callers can distinguish
      # pre-dose rows from rows with an actual last dose.
      "{last_dose_time_column}" := cummax(ifelse(
        .data[[is_dose_column]],
        .data[[time_column]],
        -Inf
      ))
    ) %>%
    dplyr::ungroup()

  list(
    data = data,
    temporary_columns = temporary_columns
  )
}

#' @noRd
add_nonmem_pk_observation_column <- function(
  .data,
  evid_column,
  blq_column,
  is_pk_observation_column
) {
  # Start with the standard NONMEM observation definition.
  #
  # EVID == 0 means this row is an observation row.
  data <-
    .data %>%
    dplyr::mutate(
      "{is_pk_observation_column}" := !is.na(.data[[evid_column]]) &
        .data[[evid_column]] == 0
    )

  # If there is no resolved BLQ column, all observation rows count as usable PK.
  if (is.null(blq_column)) {
    return(data)
  }

  # If a BLQ column was found, only observations with BLQ == 0 count as usable
  # PK observations for occasion numbering.
  data %>%
    dplyr::mutate(
      "{is_pk_observation_column}" := .data[[is_pk_observation_column]] &
        !is.na(.data[[blq_column]]) &
        .data[[blq_column]] == 0
    )
}

#' @noRd
restore_nonmem_original_order <- function(.data, temporary_columns) {
  original_row_column <- temporary_columns[["mrgda_original_row"]]

  # Put rows back exactly where they started, then remove all temporary columns.
  .data %>%
    dplyr::arrange(.data[[original_row_column]]) %>%
    dplyr::select(-dplyr::all_of(temporary_columns))
}

#' @noRd
nonmem_role_is_resolved_or_disabled <- function(resolved_roles, role) {
  # "OK" means the data already has a column for this role.
  #
  # "disabled" means the user intentionally opted out of this role.
  resolved_nonmem_role_status(resolved_roles, role) %in% c("OK", "disabled")
}

#' @noRd
require_nonmem_roles_for_derivation <- function(
  resolved_roles,
  required_roles,
  derived_role_label
) {
  # Derivation helpers need actual input columns. Missing required roles are
  # reported together so the user can fix all missing inputs at once.
  missing_required_roles <- required_roles[
    vapply(
      required_roles,
      function(role) resolved_nonmem_role_status(resolved_roles, role) != "OK",
      logical(1)
    )
  ]

  if (length(missing_required_roles)) {
    stop(
      "Cannot derive ",
      derived_role_label,
      " because these required roles were not resolved: ",
      paste(missing_required_roles, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' @noRd
nonmem_time_derivation_role_columns <- function(resolved_roles) {
  # BLQ is optional. It is only used by OCC to decide which observation rows
  # count as usable PK observations.
  blq_column <- NULL

  if (resolved_nonmem_role_status(resolved_roles, "blq") == "OK") {
    blq_column <- resolved_nonmem_role_column(resolved_roles, "blq")
  }

  list(
    id = resolved_nonmem_role_column(resolved_roles, "id"),
    time = resolved_nonmem_role_column(resolved_roles, "time"),
    evid = resolved_nonmem_role_column(resolved_roles, "evid"),
    blq = blq_column
  )
}

#' @noRd
derived_nonmem_role_column_name <- function(
  role_overrides,
  role,
  default_column
) {
  # If no overrides were supplied, derive into the standard NONMEM column name.
  if (is.null(role_overrides)) {
    return(default_column)
  }

  cleaned_override <- clean_column_names(role_overrides[[role]])

  # An override to a missing column is treated as the desired output column name.
  #
  # Example:
  #
  # .role_overrides = list(tad = "MYTAD")
  #
  # means:
  #
  # Derive the missing TAD values into "MYTAD".
  if (length(cleaned_override)) {
    return(cleaned_override[[1]])
  }

  default_column
}

#' @noRd
unique_temporary_column_names <- function(
  existing_column_names,
  requested_column_names
) {
  # make.unique() appends suffixes only when a requested temporary column would
  # collide with an existing data column or a previous temporary column.
  safe_column_names <- make.unique(c(
    existing_column_names,
    requested_column_names
  ))
  stats::setNames(
    utils::tail(safe_column_names, length(requested_column_names)),
    requested_column_names
  )
}
