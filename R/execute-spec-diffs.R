#' Execute Differences Between Spec Lists
#'
#' Compare two spec-list objects and summarize added, removed, and updated
#' variables/fields.
#'
#' @param .base_spec A named list representing the previous spec-list.
#' @param .compare_spec A named list representing the current spec-list.
#'
#' @return A list with element `diffs`, a two-column tibble (`name`, `value`).
#' @noRd
execute_spec_diffs <- function(.base_spec, .compare_spec) {
  out <- list()
  out$diffs <- tibble::tibble(name = character(), value = character())

  if (is.null(.base_spec)) {
    .base_spec <- list()
  }
  if (is.null(.compare_spec)) {
    .compare_spec <- list()
  }

  base_vars <- names(.base_spec)
  compare_vars <- names(.compare_spec)

  if (is.null(base_vars)) {
    base_vars <- character()
  }
  if (is.null(compare_vars)) {
    compare_vars <- character()
  }

  added_spec_vars <- setdiff(compare_vars, base_vars)
  removed_spec_vars <- setdiff(base_vars, compare_vars)

  if (length(added_spec_vars) > 0) {
    out$diffs <- dplyr::bind_rows(
      out$diffs,
      tibble::tibble(
        name = "Spec Variables Added",
        value = paste(added_spec_vars, collapse = ", ")
      )
    )
  }

  if (length(removed_spec_vars) > 0) {
    out$diffs <- dplyr::bind_rows(
      out$diffs,
      tibble::tibble(
        name = "Spec Variables Removed",
        value = paste(removed_spec_vars, collapse = ", ")
      )
    )
  }

  spec_fields <- c("short", "type", "unit", "values", "decode")
  common_spec_vars <- intersect(base_vars, compare_vars)

  if (length(common_spec_vars) > 0) {
    spec_field_updates <- purrr::map_dfr(common_spec_vars, function(.var) {
      old_var <- .base_spec[[.var]]
      new_var <- .compare_spec[[.var]]

      changed_fields <- spec_fields[
        purrr::map_lgl(spec_fields, ~ !identical(old_var[[.x]], new_var[[.x]]))
      ]

      if (length(changed_fields) == 0) {
        return(tibble::tibble())
      }

      tibble::tibble(
        name = paste0("Spec Updated: ", .var),
        value = paste(changed_fields, collapse = ", ")
      )
    })

    out$diffs <- dplyr::bind_rows(out$diffs, spec_field_updates)
  }

  out
}
