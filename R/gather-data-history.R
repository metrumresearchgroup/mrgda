#' Gather Data History
#'
#' This function consolidates the current data history with previous revisions. It reads the differences in the metadata
#' from "diffs.csv", filters the required changes and combines this with the current revision details.
#'
#' @param .cur_history A data frame or tibble containing the current data history.
#' @param .comment A comment describing the current revision. If not provided, defaults to "No comment".
#' @param .meta_data_folder Path to the folder containing the metadata, specifically "diffs.csv".
#' @param .prev_rev A character string specifying the previous revision code or number.
#'
#' @return A combined data frame/tibble of the current revision details with the previous data history.
#'
#' @details This function operates in several steps:
#' \enumerate{
#'   \item If `.comment` is not provided, it defaults to "No comment".
#'   \item Attempts to read "diffs.csv" from the `.meta_data_folder`.
#'   \item Filters the differences read from "diffs.csv" that are new, removed, or added.
#'   \item Creates a new tibble `.history` for the current revision details.
#'   \item If differences were found, they're combined with the current revision details in `.history`.
#'   \item All columns in `.history` are coerced to character type and NAs are replaced with empty strings.
#'   \item Combines `.history` with `.cur_history` to form the final data history.
#' }
#'
#' @examples
#' \dontrun{
#' history_prev <- tibble::tibble(
#'   Author = "John",
#'   Comment = "Initial commit",
#'   Previous_Revision = "",
#'   Datetime = "2022-01-01")
#' gather_data_history(
#'   history_prev,
#'   .comment = "Updated data",
#'   .meta_data_folder = "path/to/metadata",
#'   .prev_rev = "54")
#' }
#'
#' @keywords internal
gather_data_history <- function(.cur_history, .comment = NULL, .meta_data_folder, .prev_rev) {

  if (is.null(.comment)) {
    .comment <- "No comment"
  }

  .cur_diffs <- tryCatch(
    suppressMessages(readr::read_csv(file.path(.meta_data_folder, "diffs.csv"))),
    error = identity
  )

  if (inherits(.cur_diffs, "error")) {

    .cur_diffs <- dplyr::tibble()

  } else {

    .cur_diffs <-
      .cur_diffs %>%
      dplyr::filter(
        grepl("N ", name, fixed = TRUE) |
          grepl("Removed", name, fixed = TRUE) |
          grepl("Added", name, fixed = TRUE)
      )

  }

  .history <-
    tibble::tibble(
      Author = Sys.info()[["user"]],
      Comment = .comment,
      `Previous Revision` = .prev_rev,
      Datetime = as.character(as.POSIXct(format(Sys.time()), tz = "EDT"))
    )

  if (nrow(.cur_diffs) > 0) {

    .history <-
      .history %>%
      dplyr::bind_cols(.cur_diffs %>% tidyr::pivot_wider()) %>%
      suppressMessages()

  }

  .history <- .history %>% dplyr::mutate_all(as.character)
  .history[is.na(.history)] <- ""

  .combined_history <- dplyr::bind_rows(.history, .cur_history)

  if (nrow(.combined_history) > 0){

    # Grab the first row for each revision
    .combined_history <-
      .combined_history %>%
      dplyr::group_by(`Previous Revision`) %>%
      dplyr::filter(1:dplyr::n() == 1) %>%
      dplyr::ungroup()

  }

  .combined_history

}
