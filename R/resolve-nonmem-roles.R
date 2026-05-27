#' Default column names to try for each NONMEM data field
#'
#' These defaults are intentionally conservative. Project-specific names and
#' analysis-specific names should be supplied through `.role_overrides` rather
#' than auto-detected.
#'
#' @noRd
default_role_candidates <- list(
  id = "ID",
  subject_label = c("SUBJID", "USUBJID"),
  time = "TIME",
  tad = c("TAD", "TAFD", "TSD"),
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
#' Inspects a data frame and returns which column should be used for each
#' NONMEM data field.
#'
#' Resolution order:
#'
#' 1. User override from `.role_overrides`.
#' 2. Exact match against conservative NONMEM column names.
#' 3. Case-insensitive match against conservative NONMEM column names.
#' 4. Unresolved.
#'
#' @param .data A data frame containing the NONMEM-style dataset.
#' @param .role_overrides Named list of user-supplied overrides. For each
#'   field, the value may be `NULL` or omitted for auto-detect, one column
#'   name, or `FALSE` to disable the field.
#'
#' @return A list with `roles` and `role_map`.
#'
#' @examples
#' \dontrun{
#' nm <- read_csv_dots(system.file("derived", "pk.csv", package = "mrgda"))
#'
#' res <- resolve_nonmem_roles(nm)
#' res$roles$id
#' res$role_map
#'
#' res <- resolve_nonmem_roles(
#'   nm,
#'   .role_overrides = list(
#'     subject_label = "USUBJID",
#'     datetime = "DATETIME"
#'   )
#' )
#' }
#'
#' @export
resolve_nonmem_roles <- function(.data, .role_overrides = list()) {
  cols <- names(.data)

  results <- purrr::imap(
    default_role_candidates,
    \(candidates, role) {
      resolve_role_col(
        role = role,
        cols = cols,
        override = .role_overrides[[role]],
        candidates = candidates
      )
    }
  )

  list(
    roles = purrr::map(results, "value"),
    role_map = purrr::map_dfr(
      results,
      `[`,
      c("role", "resolved", "source", "status")
    )
  )
}

#' Resolve one role to one column
#' @noRd
resolve_role_col <- function(role, cols, override, candidates) {
  if (isFALSE(override)) {
    return(role_row(role, NULL, NA_character_, "override", "disabled"))
  }

  override <- clean_role_cols(override)

  if (length(override) > 1) {
    stop(
      "Role override for `",
      role,
      "` must be one column name.",
      call. = FALSE
    )
  }

  if (length(override)) {
    hit <- override[[1]]
    found <- hit %in% cols

    return(role_row(
      role = role,
      value = if (found) hit else NULL,
      resolved = hit,
      source = "override",
      status = if (found) "OK" else "missing column"
    ))
  }

  hit <- match_role_col(candidates, cols)

  if (length(hit)) {
    return(role_row(
      role = role,
      value = hit[["col"]],
      resolved = hit[["col"]],
      source = hit[["source"]],
      status = "OK"
    ))
  }

  role_row(role, NULL, NA_character_, "not found", "skipped")
}

#' Match possible role columns against data columns
#' @noRd
match_role_col <- function(candidates, cols) {
  candidates <- clean_role_cols(candidates)

  hit <- intersect(candidates, cols)

  if (length(hit)) {
    return(c(col = unname(hit[[1]]), source = "exact name"))
  }

  idx <- match(toupper(candidates), toupper(cols), nomatch = 0)
  idx <- idx[idx > 0]

  if (length(idx)) {
    return(c(col = unname(cols[[idx[[1]]]]), source = "case-insensitive"))
  }

  NULL
}

#' Clean role column names
#' @noRd
clean_role_cols <- function(x) {
  x <- as.character(x)
  unique(x[!is.na(x) & nzchar(x)])
}

#' Build one row of role information
#' @noRd
role_row <- function(role, value, resolved, source, status) {
  list(
    role = role,
    value = value,
    resolved = resolved,
    source = source,
    status = status
  )
}
