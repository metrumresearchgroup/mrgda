#' Check if visit and nominal time available if datetime is missing
#'
#' @export
sdtm_check_nom_datetime <- function(.domain_df,
                                    .domain_name,
                                    .nomtime_cols = NULL,
                                    .datetime_col = NULL,
                                    .subject_col = "USUBJID",
                                    .domain_filter = NULL) {

  # Apply given filter to source domain
  if(!is.null(.domain_filter)) {
    .domain_filter <- paste0("dplyr::filter(.domain_df, ", .domain_filter, ")")
    .domain_df <- rlang::parse_expr(.domain_filter) %>% rlang::eval_tidy()
  }

  domain_lookup <- get_sdtm_lookup()
  domain_in_lookup <- .domain_name %in% domain_lookup$DOMAIN

  # Search for default datetime column if none provided
  if(is.null(.datetime_col)) {

    get_domain_value = domain_lookup %>%
      dplyr::filter(DOMAIN == .domain_name) %>%
      dplyr::pull(TIME_COL)

    assertthat::assert_that(
      !is.na(get_domain_value),
      msg = "No default time column provided for .domain_name. Please define .datetime_col"
    )

    .datetime_col <- get_domain_value
  }

  # Search for default nomtime column if none provided
  if(is.null(.nomtime_cols)) {
    .nomtime_cols <- names(.domain_df[grepl("VIS", names(.domain_df), ignore.case = TRUE)])

    assertthat::assert_that(
      length(.nomtime_cols) > 0,
      msg = "No default nominal time columns found in .domain_name. Please define .nomtime_cols"
    )
  }

  # Check if .nomtime_cols actually exist in data
  if(!is.null(.nomtime_cols)){
    assertthat::assert_that(
      all(.nomtime_cols %in% names(.domain_df)),
      msg = "All columns in .nomtime_cols are not in .domain_df"
    )
  }

  # Check if .datetime_col actually exist in data
  if(!is.null(.datetime_col)){
    assertthat::assert_that(
      all(.datetime_col %in% names(.domain_df)),
      msg = "All columns in .datetime_col are not in .domain_df"
    )
  }

  execute_nom_datetime(
    .data = .domain_df,
    .nomtime_cols = .nomtime_cols,
    .datetime_col = .datetime_col,
    .subject_col = .subject_col,
    .domain_name = .domain_name
  )

}

execute_nom_datetime <- function(.data,
                                 .nomtime_cols,
                                 .datetime_col,
                                 .subject_col,
                                 .domain_name) {

  return_list <- list()
  return_list$Name <- paste0(.domain_name, " available nomtime when missing datetime record check")

  missing_datetime <-
    .data %>%
    dplyr::select(dplyr::all_of(c(.datetime_col, .subject_col, .nomtime_cols))) %>%
    dplyr::filter(is.na(!!sym(.datetime_col)) | stringr::str_length(!!sym(.datetime_col)) == 0)

  max_able_to_fix <- 0

  for (col.i in .nomtime_cols) {

    able_to_fix <-
      missing_datetime %>%
      dplyr::select(dplyr::all_of(c(.subject_col, col.i))) %>%
      dplyr::filter(!is.na(!!sym(col.i)) | stringr::str_length(!!sym(col.i)) != 0)

    if (nrow(able_to_fix) > max_able_to_fix) {
      max_able_to_fix <- nrow(able_to_fix)
    }
  }

  return_list$NRecordsNomtimeAvailable <- paste0(max_able_to_fix, "/", nrow(missing_datetime))

  if (nrow(missing_datetime) == max_able_to_fix) {
    return_list$Result <- "Pass"
  } else {
    return_list$Result <- "Fail"
  }

  return(return_list)

}
