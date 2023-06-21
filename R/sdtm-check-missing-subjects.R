#' Check if subjects in one domain are missing in others
#'
#' @param .domain_df1 First source data domain to compare (data.frame)
#' @param .domain_df2 Second source data domain to compare (data.frame)
#' @param .domain_name1 Name of first source domain (character)
#' @param .domain_name2 Name of second source domain (character)
#' @param .subject_col Column name of subject identifier (character, default = "USUBJID")
#' @param .domain_filter1 Code to use inside a dplyr::filter on the first source data.frame (optional)
#' @param .domain_filter2 Code to use inside a dplyr::filter on the second source data.frame (optional)
#'
#' @export
sdtm_check_missing_subjects <- function(.domain_df1,
                                        .domain_df2,
                                        .domain_name1,
                                        .domain_name2,
                                        .subject_col = "USUBJID",
                                        .domain_filter1 = NULL,
                                        .domain_filter2 = NULL) {

  # Apply filter to first domain (if provided)
  if(!is.null(.domain_filter1)) {
    .domain_filter1 <- paste0("dplyr::filter(.domain_df1, ", .domain_filter1, ")")
    .domain_df1 <- rlang::parse_expr(.domain_filter1) %>% rlang::eval_tidy()
  }

  # Apply filter to second domain (if provided)
  if(!is.null(.domain_filter2)) {
    .domain_filter2 <- paste0("dplyr::filter(.domain_df2, ", .domain_filter2, ")")
    .domain_df2 <- rlang::parse_expr(.domain_filter2) %>% rlang::eval_tidy()
  }

  if (!.subject_col %in% names(.domain_df1)) {
    stop(paste0(.subject_col, " not found in .domain_df1"))
  }
  if (!.subject_col %in% names(.domain_df2)) {
    stop(paste0(.subject_col, " not found in .domain_df2"))
  }

  subjects_1 <- unique(.domain_df1[[.subject_col]])
  subjects_2 <- unique(.domain_df2[[.subject_col]])
  n_df1Only <- length(subjects_1[!subjects_1 %in% subjects_2])
  n_df2Only <- length(subjects_2[!subjects_2 %in% subjects_1])

  output_list <- list()
  output_list$Name <- paste0(.domain_name1, " and ", .domain_name2, " missing subjects check")
  output_list$Result <- ifelse(n_df1Only == 0 & n_df2Only == 0, "Pass", "Fail")
  output_list$NSubjectsBoth <- length(subjects_1[subjects_1 %in% subjects_2])
  df1name <- paste0("N", .domain_name1, "Only")
  output_list[[df1name]] <- n_df1Only
  df2name <- paste0("N", .domain_name2, "Only")
  output_list[[df2name]] <- n_df2Only

  return(output_list)

}
