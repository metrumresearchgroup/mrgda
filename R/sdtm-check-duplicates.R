#' Check if duplicates exist in SDTM domain
#'
#' @param .domain_df source data (data.frame)
#' @param .domain_name Name of source domain (character)
#' @param .cols_check Columns to search duplicates across (ie. c("USUBJID", "PCDTC"))
#' @param .subject_col Column name of subject identifier (character)
#' @param .domain_filter Code to use inside a dplyr::filter on the source data.frame before duplicates check
#'
#' @examples
#'\dontrun{
#' sdtm_check_duplicates(.domain_df = src_list$dm, .domain_name = "dm", .cols_check = NULL, .subject_col = "USUBJID", .domain_filter = NULL)
#' sdtm_check_duplicates(.domain_df = src_list$ex, .domain_name = "ex", .cols_check = c("USUBJID", "EXSTDTC", "EXTRT"))
#' sdtm_check_duplicates(.domain_df = src_list$pc, .domain_name = "pc", .domain_filter = "PCSTAT != 'NOT DONE'")
#'}
#' @export
sdtm_check_duplicates <- function(.domain_df,
                                  .domain_name,
                                  .cols_check = NULL,
                                  .subject_col = "USUBJID",
                                  .domain_filter = NULL) {

  if(!is.null(.domain_filter)) {
    .domain_filter <- paste0("dplyr::filter(.domain_df, ", .domain_filter, ")")
    .domain_df <- rlang::parse_expr(.domain_filter) %>% rlang::eval_tidy()
  }

  domain_lookup <- get_sdtm_lookup()
  domain_in_lookup <- .domain_name %in% domain_lookup$DOMAIN

  if(is.null(.cols_check)) {

    assertthat::assert_that(
      domain_in_lookup,
      msg = "No default columns provided for .domain_name. Please define .cols_check"
    )

    get_domain_value = domain_lookup %>%
      dplyr::filter(DOMAIN == .domain_name) %>%
      dplyr::pull(ALL_COLS)

    .cols_check <- stringr::str_split_fixed(get_domain_value, ",", n=Inf)[1,]
    .cols_check <- .cols_check[.cols_check %in% names(.domain_df)]

    assertthat::assert_that(
      length(.cols_check) > 0,
      msg = "No default columns for .domain_name found in .domain_df. Please define .cols_check"
    )
  }

  # Check if cols_check actually exist in data
  assertthat::assert_that(
    all(.cols_check %in% names(.domain_df)),
    msg = "All columns in .cols_check are not in .domain_df"
  )

  # Drop all blank or NA rows so that they are not caught as dups
  for(col.i in .cols_check) {
    .domain_df[[col.i]] <- as.character(.domain_df[[col.i]])
    .domain_df[[col.i]] <- tidyr::replace_na(.domain_df[[col.i]], "")
    keep.i <- nchar(trimws(.domain_df[[col.i]])) > 0
    .domain_df <- .domain_df[keep.i, ]
  }

  output_list <-
    execute_dups(
      .data = .domain_df,
      .cols_check = .cols_check,
      .subject_col = .subject_col,
      .domain_name = .domain_name
    )

  return(output_list)

}

#' @keywords internal
execute_dups <- function(.data,
                         .cols_check,
                         .subject_col,
                         .domain_name) {

  return_list <- list()
  return_list$Name <- paste0(.domain_name, " duplicate record check")

  test_df <-
    .data %>%
    dplyr::select(dplyr::all_of(.cols_check)) %>%
    dplyr::group_by_at(.cols_check) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1)

  if (nrow(test_df) == 0) {
    return_list$Result <- "Pass"
    return_list$NRecordsFail <- NULL
    return_list$IssueRecords <- NULL
  } else {
    return_list$Result <- "Fail"
    return_list$NRecordsFail <- paste0(nrow(test_df), "/", nrow(.data))
    return_list$IssueRecords <- test_df
  }

  return(return_list)

}
