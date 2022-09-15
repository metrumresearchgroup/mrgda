gather_flags <- function(.spec){

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

    yspec::pull_meta(.spec, "flags")[recognized_flags] %>%
    purrr::set_names(recognized_flags)
}
