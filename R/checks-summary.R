#' @keywords internal
checks_summary <- function(.data,
                           .test_df,
                           .cols_check,
                           .domain_name,
                           .check_type) {

  return_list <- list()
  return_list$Name <- paste0(.domain_name, " ", .check_type)
  return_list$ColsCheck <- .cols_check
  n_fail <- nrow(.test_df)

  if(n_fail == 0){
    return_list$Result <- "Pass"
    return(return_list)
  }

  .test_df$MRGDA_ISSUE_ID <-
    if (dplyr::is.grouped_df(.test_df)) {
      dplyr::group_indices(.test_df)
    } else {
      1:nrow(.test_df)
    }

  .test_df <-
    .test_df %>%
    dplyr::ungroup() %>%
    dplyr::arrange(MRGDA_ISSUE_ID) %>%
    dplyr::relocate(MRGDA_ISSUE_ID)


  pct_of_data <- round(n_fail/nrow(.data)*100,2)

  return_list$Result <- "Fail"
  return_list[["N fail (%)"]] <- paste0(n_fail, " (", pct_of_data, ")")
  return_list$IssueRecords <- .test_df

  return(return_list)
}
