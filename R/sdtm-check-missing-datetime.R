#' Check if missing datetimes in SDTM domain
#'
#' @param .domain_df source data (data.frame)
#' @param .domain_name Name of source domain (character)
#' @param .col_check Column to search for missing date/times (ie. c("EXSTDTC"))
#' @param .time_col Column name of subject identifier (character)
#' @param .domain_filter Code to use inside a dplyr::filter on the source data.frame before duplicates check
#'
#' @examples
#'\dontrun{
#' sdtm_check_missing_datetime(.domain_df = src_list$pc, .domain_name = "pc", .domain_filter = "PCSTAT != 'NOT DONE'")
#' sdtm_check_missing_datetime(.domain_df = src_list$ex, .domain_name = "ex")
#'}
#' @export
sdtm_check_missing_datetime <- function(.domain_df,
                                        .domain_name,
                                        .time_col = NULL,
                                        .subject_col = "USUBJID",
                                        .domain_filter = NULL) {

  if(!is.null(.domain_filter)) {
    .domain_filter <- paste0("dplyr::filter(.domain_df, ", .domain_filter, ")")
    .domain_df <- rlang::parse_expr(.domain_filter) %>% rlang::eval_tidy()
  }

  domain_lookup <- get_sdtm_lookup()
  domain_in_lookup <- .domain_name %in% domain_lookup$DOMAIN

  if(is.null(.time_col)) {

    assertthat::assert_that(
      domain_in_lookup,
      msg = "No default column for provided for .domain_name. Please define .time_col"
    )

    get_domain_value = domain_lookup %>%
      dplyr::filter(DOMAIN == .domain_name) %>%
      dplyr::pull(TIME_COL)

    assertthat::assert_that(
      !is.na(get_domain_value),
      msg = "No default time column provided for .domain_name. Please define .time_col"
    )

    .time_col <- get_domain_value
  }

  # Check if cols_check actually exist in data
  if(!is.null(.time_col)){
    assertthat::assert_that(
      all(.time_col %in% names(.domain_df)),
      msg = ".time_col is not in .domain_df, please define .time_col"
    )
  }

  output_list <-
    execute_missing(
      .data = .domain_df,
      .cols_check = .time_col,
      .subject_col = .subject_col,
      .domain_name = .domain_name
    )

  return(output_list)

}

#' @keywords internal
execute_missing <- function(.data,
                            .cols_check,
                            .subject_col,
                            .domain_name) {

  return_list <- list()
  return_list$Name <- paste0(.domain_name, " missing record check")

  test_df <-
    .data %>%
    dplyr::select(dplyr::all_of(c(.cols_check, .subject_col))) %>%
    dplyr::filter(is.na(!!sym(.cols_check)) | stringr::str_length(!!sym(.cols_check)) == 0)

  return_list$NRecordsFail <- paste0(nrow(test_df), "/", nrow(.data))

  if (nrow(test_df) > 0) {
    test_df <-
      test_df %>%
      dplyr::count(!!sym(.subject_col))
  }

  if (nrow(test_df) == 0) {
    return_list$Result <- "Pass"
    return_list$IssueRecords <- NULL
  } else {
    return_list$Result <- "Fail"
    return_list$IssueRecords <- test_df
  }

  return(return_list)

}

