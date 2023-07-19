#' Check for outliers in dataset
#'
#' @description
#' Check data frame for outliers and extreme outliers.
#' Outliers are defined as values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR.
#' Extreme outliers are defined as values above Q3 + 3xIQR or below Q1 - 3xIQR.
#'
#' @param .df data frame
#' @param .group Column name(s) to group by. Default is NULL
#' @param .time Column name of datetime variable. Default is NULL
#' @param .cov Column name(s) to be assessed for outliers. Default is all numeric columns in the dataset
#' @param .show_mean_med Include mean and median in output. Default is TRUE
#'
#' @examples
#' get_outliers(.df = src_list$lb, .group = "LBTESTCD", .time = "LBDTC", .cov = "LBSTRESN")
#' @export

get_outliers <- function(.df, .group, .id= "USUBJID", .time= NULL, .cov="ALL", .show_mean_med = TRUE){

  if (.cov == "ALL"){
    .cov <- names(dplyr::select_if(.df, is.numeric))
  } else {
    .cov <- .cov
  }

  .outliers <- dplyr::bind_rows(
    purrr::map(.cov,
               ~ make_outlier_df(.df = df, .group = .group, .time = .time, .cov = .x)
    ) %>%
      setNames(.cov))

  .outliers
}
