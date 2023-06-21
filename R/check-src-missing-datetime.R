#' Check if missing datetimes in SDTM domain
#'
#' @param .domain_df source data (data.frame)
#' @param .domain_name Name of source domain (character)
#' @param .time_col Column name of datetime variable (character)
#' @param .subject_col Column name of subject identifier (character)
#'
#' @examples
#'\dontrun{
#' check_src_missing_datetime(.domain_df = src_list$pc, .domain_name = "pc")
#' check_src_missing_datetime(.domain_df = src_list$ex, .domain_name = "ex")
#'}
#' @export
check_src_missing_datetime <- function(.domain_df,
                                        .domain_name,
                                        .time_col = NULL,
                                        .subject_col = "USUBJID") {

  domain_lookup <- get_sdtm_lookup(.domain_name)

  if(is.null(.time_col)) {
    .time_col <- domain_lookup$TIME_COL
  }

  if (is.null(.time_col)) {
    return(NULL)
  }

  # Check if cols_check actually exist in data
  assertthat::assert_that(
    all(.time_col %in% names(.domain_df)),
    msg = ".time_col is not in .domain_df, please define .time_col"
  )

  test_df <-
    .domain_df %>%
    dplyr::filter(is.na(!!sym(.time_col)) | stringr::str_length(!!sym(.time_col)) == 0)

  output_list <-
    checks_summary(
      .data = .domain_df,
      .test_df = test_df,
      .cols_check = .time_col,
      .domain_name = .domain_name,
      .check_type = "missing datetime record check"
    )

  return(output_list)

}


