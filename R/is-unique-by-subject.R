#' @keywords internal
is_unique_by_subject <- function(.df, .column) {
  nrow(.df %>% dplyr::count(!!sym(.column)) %>% dplyr::filter(n > 1)) == 0
}
