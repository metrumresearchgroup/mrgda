#' @keywords internal
make_outlier_df <- function(.df, .group = NULL, .id = "USUBJID", .time = NULL, .cov = "ALL", .show_mean_med = TRUE ){

  if (.show_mean_med = TRUE){
    .df <- .df %>%
      select(all_of(.id), .group, .time, .cov) %>%
      group_by(!!! dplyr::syms(.group)) %>%
      mutate(MEAN = mean(!!dplyr::sym(.cov)),
             MEDIAN = median(!!dplyr::sym(.cov))
      ) %>%
      rstatix::identify_outliers(.cov) %>%
      dplyr::ungroup()
  } else {
    .df <- .df %>%
      select(all_of(.id), .group, .time, .cov) %>%
      group_by(!!! dplyr::syms(.group)) %>%
      rstatix::identify_outliers(.cov) %>%
      dplyr::ungroup()
  }

  .df
}
