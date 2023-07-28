#' Search for ID columns across list of dataframes
#'
#' @inheritParams src_viz
#' @param id_cols vector of id columns to search for
#'
#' @importFrom tidyselect all_of
#'
#' @keywords internal
check_subject_col <- function(.src_list, id_cols = c("ID", "USUBJID")){
  # Look for USUBJID and ID columns across datasets
  id_col_df <- purrr::map_dfr(.src_list, function(df){
    purrr::map_lgl(id_cols, function(id_col){
      any(grepl(glue("^(?i){id_col}$"), names(df)))
    }) %>% stats::setNames(id_cols)
  }) %>% dplyr::mutate(dataset = names(.src_list)) %>%
    tidyr::pivot_longer(all_of(id_cols), names_to = "id_col", values_to = "present")

  if(!any(id_col_df$present)){
    # Set to NULL if none are found
    subject_col <- NULL
  }else{
    # Use the id_col with the most occurrences across the datasets
    id_count <- id_col_df %>% dplyr::count(.data$id_col, .data$present)
    subject_col <- id_count$id_col[id_count$n == max(id_count$n)]
    # Use USUBJID if they are the same frequency
    if(length(subject_col) > 1){
      subject_col <- "USUBJID"
    }
  }

  return(subject_col)
}
