#' Summarize Derived Data
#'
#' Creates a summary table from a nested list of data frames.
#' Each element of `.derived_list` is treated as a derivation
#' type, and each nested element is expected to be a data frame.
#'
#' The returned data frame contains:
#' \itemize{
#'   \item \code{df_name} - the name of each derived data frame
#'   \item \code{type} - the parent grouping/type name
#'   \item \code{columns} - a comma-separated list of column names
#' }
#'
#' @param .derived_list A named list of lists containing data frames.
#'   The outer list names represent derivation types, and the inner
#'   list names represent individual data frame names.
#'
#' @examples
#' derived <- list(
#'   sl = list(
#'     adsl = data.frame(ID = 1:3, TRT = c("A", "B", "A")),
#'     adae = data.frame(ID = 1:2, AE = c("Headache", "Nausea"))
#'   ),
#'   tv = list(
#'     qc_adsl = data.frame(ID = 1:3, FLAG = c("Y", "N", "Y"), TIME = c(0, 2, 4))
#'   )
#' )
#'
#' derived_to_here(derived)
#'
#' @export
derived_to_here <- function(.derived_list) {
  do.call(rbind, lapply(names(.derived_list), function(type) {
    sublist <- .derived_list[[type]]

    data.frame(
      df_name = names(sublist),
      type = type,
      columns = sapply(sublist, function(df) paste(names(df), collapse = ", ")),
      stringsAsFactors = FALSE
    )
  }))
}
