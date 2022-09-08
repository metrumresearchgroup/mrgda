#' Provide diagnostic summary of an NMTRAN dataset
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @examples
#'
#'
#' @md
#' @export
nm_validate <- function(.data, .spec){
  # gather flags ------------------------------------------------------------
  recognized_flags <-
    c(
      "id",
      "study",
      "primary_keys",
      "time",
      "bl_cov_cat",
      "bl_cov_cont",
      "tv_cov_cat",
      "tv_cov_cont"
    )

  flags <-
    yspec::pull_meta(.spec, "flags")[recognized_flags] %>%
    purrr::set_names(recognized_flags)


}
