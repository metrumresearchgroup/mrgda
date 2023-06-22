#' Check if duplicates exist in source data domain
#'
#' @description
#' Checks if any duplicates records exist in a given data.frame. Some commonly used
#' domains have default columns to use for the duplicates check, however the .cols_check
#' argument can be used to customize the columns to be used for the duplicate check.
#'
#' @param .domain_df source data (data.frame)
#' @param .domain_name Name of source domain (character)
#' @param .cols_check Columns to search duplicates across (ie. c("USUBJID", "PCDTC"))
#' @param .subject_col Column name of subject identifier (character)
#'
#' @examples
#'\dontrun{
#' check_src_duplicates(
#'   .domain_df = src_list$dm,
#'   .domain_name = "dm",
#'   .cols_check = NULL,
#'   .subject_col = "USUBJID")
#' check_src_duplicates(
#'   .domain_df = src_list$ex,
#'   .domain_name = "ex",
#'   .cols_check = c("USUBJID", "EXSTDTC", "EXTRT"))
#' check_src_duplicates(
#'   .domain_df = src_list$pc,
#'   .domain_name = "pc")
#'}
#' @export
check_src_duplicates <- function(.domain_df,
                                  .domain_name,
                                  .cols_check = NULL,
                                  .subject_col = "USUBJID") {

  domain_lookup <- get_sdtm_lookup(.domain_name)

  if(is.null(.cols_check)){
    .cols_check <- c(domain_lookup$UNIQUE_COLS, domain_lookup$TIME_COL)
  }

  # If it's still NULL, exit function
  if(is.null(.cols_check)){
    return(NULL)
  }

  .cols_check <- c(.cols_check, .subject_col)

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

  test_df <-
    .domain_df %>%
    dplyr::group_by_at(.cols_check) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  output_list <-
    checks_summary(
      .data = .domain_df,
      .test_df = test_df,
      .cols_check = .cols_check,
      .domain_name = .domain_name,
      .check_type = "duplicate record check"
    )

  return(output_list)

}
