#' Gather Data History
#'
#' This function takes the current data, its associated history, and additional
#' metadata to record a new entry in the data's revision history. It provides
#' a systematic way to keep track of changes, the author of the change, and more.
#'
#' @param .cur_data A data frame containing the current version of the data.
#' @param .cur_history A data frame containing the current history/revision log of the data.
#' @param .comment A character string representing a comment or note about the current revision.
#'        Defaults to "No comment" if not provided.
#' @param .prev_rev A value representing the previous revision or version number. If not provided,
#'        it won't be logged.
#' @keywords internal
gather_data_history <- function(.cur_data, .cur_history, .comment = NULL, .prev_rev) {

  if (is.null(.comment)) {
    .comment <- "No comment"
  }

  .history <-
    tibble::tibble(
      Author = Sys.info()[["user"]],
      `N Row` = nrow(.cur_data),
      `N Col` = ncol(.cur_data),
      `N ID` = length(unique(.cur_data[["ID"]])),
      `Previous Revision` = .prev_rev,
      `Datetime (EDT)` = as.character(as.POSIXct(format(Sys.time()), tz = "EDT")),
      Comment = .comment
    )

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
