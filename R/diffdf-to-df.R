#' Convert diffdf object to data frame
#'
#' This function takes a diffdf object and converts it into a dataframe. Only the columns
#' with names starting with "VarDiff_" are included. All data is transformed into character type,
#' and the "..ROWNUMBER.." column is renamed to "ROWNUMBER".
#'
#' @param .diffdf_obj A diffdf object. If the provided object is not a diffdf, the function will stop.
#'
#' @return A dataframe containing the differences from the input diffdf object. Each column
#'   corresponds to a variable from the original datasets, and each row represents a unique
#'   observation. The dataframe includes only the columns from the diffdf object that start
#'   with "VarDiff_", all columns are converted to character type, and "..ROWNUMBER.." is renamed to "ROWNUMBER".
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(A = c(1,2,3), B = c(4,5,6))
#' df2 <- data.frame(A = c(1,2,4), B = c(4,5,7))
#' diffdf_obj <- diffdf::diffdf(df1, df2)
#' df <- diffdf_to_df(diffdf_obj)
#' }
#'
#' @export
diffdf_to_df <- function(.diffdf_obj){

  if (!inherits(.diffdf_obj, "diffdf")) {
    stop(".diffdf_obj must be a diffdf object")
  }

  .var_diffs <- grepl("VarDiff_", names(.diffdf_obj), fixed = TRUE)

  if (!any(.var_diffs)) {
    return(NULL)
  }

  .diffdf_obj[names(.diffdf_obj)[.var_diffs]][[1]] %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::rename(ROWNUMBER = "..ROWNUMBER..")

}
