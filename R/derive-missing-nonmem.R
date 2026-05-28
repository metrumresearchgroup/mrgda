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
  resolved_roles <- resolve_nonmem_roles(
    .data = .data,
    .role_overrides = .role_overrides
  )

  if (nonmem_role_is_resolved_or_disabled(resolved_roles, "tad")) {
    return(.data)
  }

  require_nonmem_roles_for_derivation(
    resolved_roles = resolved_roles,
    required_roles = c("id", "time", "evid"),
    derived_role_label = "TAD"
  )

  role_columns <- nonmem_time_derivation_role_columns(resolved_roles)
  tad_column <- derived_nonmem_role_column_name(
    role_overrides = .role_overrides,
    role = "tad",
    default_column = "TAD"
  )

  prepared_data <- prepare_nonmem_dose_interval_data(
    .data = .data,
    role_columns = role_columns,
    protected_column_names = tad_column
  )

  temporary_columns <- prepared_data$temporary_columns
  time_column <- temporary_columns[["mrgda_time"]]
  last_dose_time_column <- temporary_columns[["mrgda_last_dose_time"]]

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
  resolved_roles <- resolve_nonmem_roles(
    .data = .data,
    .role_overrides = .role_overrides
  )

  if (nonmem_role_is_resolved_or_disabled(resolved_roles, "occasion")) {
    return(.data)
  }

  require_nonmem_roles_for_derivation(
    resolved_roles = resolved_roles,
    required_roles = c("id", "time", "evid"),
    derived_role_label = "OCC"
  )

  role_columns <- nonmem_time_derivation_role_columns(resolved_roles)
  occ_column <- derived_nonmem_role_column_name(
    role_overrides = .role_overrides,
    role = "occasion",
    default_column = "OCC"
  )

  prepared_data <- prepare_nonmem_dose_interval_data(
    .data = .data,
    role_columns = role_columns,
    protected_column_names = occ_column
  )

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

  data <-
    .data %>%
    dplyr::mutate(
      "{original_row_column}" := dplyr::row_number(),
      "{time_column}" := as.numeric(.data[[original_time_column]]),
      "{is_dose_column}" := !is.na(.data[[original_evid_column]]) &
        .data[[original_evid_column]] == 1,
      "{dose_sort_column}" := ifelse(.data[[is_dose_column]], 0L, 1L)
    ) %>%
    dplyr::arrange(
      .data[[original_id_column]],
      .data[[time_column]],
      .data[[dose_sort_column]],
      .data[[original_row_column]]
    ) %>%
    dplyr::group_by(.data[[original_id_column]]) %>%
    dplyr::mutate(
      "{n_doses_column}" := cumsum(.data[[is_dose_column]]),
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
  data <-
    .data %>%
    dplyr::mutate(
      "{is_pk_observation_column}" := !is.na(.data[[evid_column]]) &
        .data[[evid_column]] == 0
    )

  if (is.null(blq_column)) {
    return(data)
  }

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

  .data %>%
    dplyr::arrange(.data[[original_row_column]]) %>%
    dplyr::select(-dplyr::all_of(temporary_columns))
}

#' @noRd
nonmem_role_is_resolved_or_disabled <- function(resolved_roles, role) {
  resolved_nonmem_role_status(resolved_roles, role) %in% c("OK", "disabled")
}

#' @noRd
require_nonmem_roles_for_derivation <- function(
  resolved_roles,
  required_roles,
  derived_role_label
) {
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
  if (is.null(role_overrides)) {
    return(default_column)
  }

  cleaned_override <- clean_column_names(role_overrides[[role]])

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
  safe_column_names <- make.unique(c(
    existing_column_names,
    requested_column_names
  ))
  stats::setNames(
    utils::tail(safe_column_names, length(requested_column_names)),
    requested_column_names
  )
}
