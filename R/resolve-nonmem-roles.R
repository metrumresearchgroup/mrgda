#' Default candidate column names for each NONMEM semantic role
#'
#' @keywords internal
default_role_candidates <- list(
  id = c("ID", "SUBJID", "SUBJECT", "CID"),
  subject_label = c("USUBJID", "SUBJID", "SUBJECT"),
  time = c("TIME", "TIMEA", "TFRD", "NTIME", "ATIME"),
  tad = c("TAD", "TAFD", "TSD", "TIMEAD"),
  dv = c("DV", "CONC", "CP", "Y", "LIDV", "IPRED"),
  evid = c("EVID", "EVENT", "EVENTID"),
  mdv = c("MDV"),
  amt = c("AMT", "DOSEAMT", "DAMT"),
  rate = c("RATE"),
  dur = c("DUR", "DURATION"),
  cmt = c("CMT", "COMP"),
  dvid = c("DVID", "DVIDN", "DVIDC", "PARAMCD"),
  blq = c("BLQ", "BLOQ", "BQL", "LLOQFL", "BLQFL"),
  dose_group = c("DOSE", "DOSEGRP", "TRT", "TRTA", "TRTAN", "ARM", "ARMCD"),
  day = c("DAY", "STDY", "NOMDAY", "ADY"),
  datetime = c("DATETIME", "DTC", "DATE", "LBDTC", "PCDTC"),
  comment = c("C", "COMMENT", "EXCL", "DROP", "IGNORE", "FLAG"),
  occasion = c("OCC", "VISIT", "VISITNUM", "PERIOD", "CYCLE", "OCCASION")
)

.scalar_roles <- names(default_role_candidates)

.multi_roles <- c(
  "bl_cat_cov",
  "bl_cont_cov",
  "tv_cat_cov",
  "tv_cont_cov",
  "occ_cov"
)

#' Resolve semantic roles for a NONMEM-style dataset
#'
#' Inspects a data frame plus its yspec specification and returns which
#' column should fill each NONMEM semantic role (`id`, `time`, `dv`, `amt`,
#' covariate groups, etc.). Resolution order for each role:
#'
#' 1. User override from `.role_overrides`.
#' 2. yspec `SETUP__$flags` (for multi-column covariate roles).
#' 3. Exact match against the default NONMEM column names for that role.
#' 4. Case-insensitive match against the default NONMEM column names.
#' 5. Unresolved (`NULL` for scalar roles, `character()` for multi-column roles).
#'
#' @param .data A data frame containing the NONMEM-style dataset.
#' @param .spec A yspec object from `yspec::load_spec()`.
#' @param .role_overrides Named list of user-supplied overrides. For each
#'   role, the value may be `NULL` or omitted (auto-detect), a column name
#'   (use that column), `FALSE` (disable the role), `character()` (clear a
#'   multi-column role), or a character vector of column names (multi-column
#'   roles).
#'
#' @return A list with `roles` (named list of resolved roles keyed by role
#'   name) and `role_map` (tibble with columns `role`, `resolved`, `source`,
#'   `status`).
#' @examples
#' \dontrun{
#' spec <- yspec::load_spec("inst/examples/pk.yml")
#' nm <- read_csv_dots("inst/examples/pk.csv")
#' res <- resolve_nonmem_roles(.data = nm, .spec = spec)
#' res$roles$id
#' res$role_map
#' }
#' @md
#' @export
resolve_nonmem_roles <- function(.data, .spec, .role_overrides = list()) {
  cols <- names(.data)
  flags <- tryCatch(yspec::get_meta(.spec)$flags, error = function(e) NULL) %||%
    list()

  results <- c(
    purrr::map(
      .scalar_roles,
      ~ resolve_scalar_role(.x, cols, .role_overrides[[.x]])
    ),
    purrr::map(
      .multi_roles,
      ~ resolve_multi_role(.x, cols, .role_overrides[[.x]], flags[[.x]])
    )
  ) %>%
    purrr::set_names(c(.scalar_roles, .multi_roles))

  list(
    roles = purrr::map(results, "value"),
    role_map = tibble::tibble(
      role = unname(purrr::map_chr(results, "role")),
      resolved = unname(purrr::map_chr(results, "resolved")),
      source = unname(purrr::map_chr(results, "source")),
      status = unname(purrr::map_chr(results, "status"))
    )
  )
}

#' Resolve a single scalar role
#' @keywords internal
resolve_scalar_role <- function(role, cols, override) {
  if (isFALSE(override)) {
    return(role_row(role, NULL, NA_character_, "override", "disabled"))
  }
  if (is.character(override) && length(override) && nzchar(override[1])) {
    hit <- override[1]
    return(role_row(
      role,
      if (hit %in% cols) hit else NULL,
      hit,
      "override",
      if (hit %in% cols) "OK" else "missing column"
    ))
  }

  candidates <- default_role_candidates[[role]]
  if (length(hit <- intersect(candidates, cols))) {
    return(role_row(role, hit[1], hit[1], "exact name", "OK"))
  }
  if (length(hit_ci <- cols[toupper(cols) %in% toupper(candidates)])) {
    return(role_row(
      role,
      hit_ci[1],
      hit_ci[1],
      "case-insensitive",
      "OK"
    ))
  }

  role_row(role, NULL, NA_character_, "not found", "skipped")
}

#' Resolve a single multi-column role
#' @keywords internal
resolve_multi_role <- function(role, cols, override, flagged) {
  if (isFALSE(override)) {
    return(role_row(
      role,
      character(),
      NA_character_,
      "override",
      "disabled"
    ))
  }
  if (is.character(override)) {
    return(role_row_multi(
      role,
      intersect(override, cols),
      override,
      "override"
    ))
  }
  if (length(flagged)) {
    flagged <- as.character(flagged)
    return(role_row_multi(
      role,
      intersect(flagged, cols),
      flagged,
      "yspec flags"
    ))
  }
  role_row(role, character(), NA_character_, "not found", "skipped")
}

#' Build a role-row list (scalar)
#' @keywords internal
role_row <- function(role, value, resolved, source, status) {
  list(
    role = role,
    value = value,
    resolved = resolved,
    source = source,
    status = status
  )
}

#' Build a role-row list (multi-column), computing status from requested vs kept
#' @keywords internal
role_row_multi <- function(role, kept, requested, source) {
  missing <- setdiff(requested, kept)
  role_row(
    role,
    kept,
    if (length(kept)) paste(kept, collapse = ", ") else NA_character_,
    source,
    if (length(missing) == 0) {
      "OK"
    } else {
      glue::glue("missing: {paste(missing, collapse = ', ')}")
    }
  )
}
