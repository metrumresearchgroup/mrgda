#' @keywords internal
diff_df <- function(.base, .compare, .file){
  diffdf::diffdf(
    base = .base,
    compare = .compare,
    file = .file,
    suppress_warnings = TRUE
  )
}
