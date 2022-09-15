gather_flags <- function(.data, .spec){

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

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags] %>%
    purrr::set_names(recognized_flags)

  # Check if all flags are NULL
  .flags_bin <- purrr::map(.flags, ~ is.null(.x))
  if (all(.flags_bin == TRUE)) {
    stop("No flags specified in spec file")
  }

  # Modify empty flags with dummy value & create matching column in data set

}
