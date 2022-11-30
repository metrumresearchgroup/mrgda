gather_flags <- function(.data, .spec){

  recognized_flags <- tibble::tribble(
    ~name, ~type,
    "id", "group",
    "study", "group",
    "primary_keys", "group",
    "time", "group",
    "bl_cat_cov", "group",
    "bl_cont_cov", "calculation",
    "tv_cat_cov", "group",
    "tv_cont_cov", "calculation",
    "num", "group"
  )

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags$name] %>%
    purrr::set_names(recognized_flags$name)

  # Check if all flags are NULL
  .flags_bin <- purrr::map(.flags, ~ is.null(.x))
  if (all(.flags_bin == TRUE)) {
    stop("No flags found in spec file")
  }

  list_return <- list()

  list_return[["data"]] <- .data
  list_return[["flags"]] <- .flags

  return(list_return)

}
