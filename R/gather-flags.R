gather_flags <- function(.data, .spec){

  recognized_flags <- tibble::tribble(
    ~name, ~type,
    "id", "group",
    "study", "group",
    "primary_keys", "group",
    "time", "group",
    "bl_cov_cat", "group",
    "bl_cov_cont", "calculation",
    "tv_cov_cat", "group",
    "tv_cov_cont", "calculation"
  )

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags$name] %>%
    purrr::set_names(recognized_flags$name)

  # Check if all flags are NULL
  .flags_bin <- purrr::map(.flags, ~ is.null(.x))
  if (all(.flags_bin == TRUE)) {
    stop("No flags found in spec file")
  }

  # Modify empty flags with dummy value & create matching column in data set
  miss_flags <- c()
  for (i in 1:length(.flags_bin)) {

    if (!.flags_bin[[i]]) {
      next
    }

    miss_flags <- append(miss_flags, names(.flags[i]))

    if (recognized_flags$type[i] == "calculation") {
      .flags[i] = paste0("mrgda_", names(.flags[i]))
      .data[[paste0("mrgda_", names(.flags[i]))]] = 0
    } else {
      .flags[i] = paste0("mrgda_", names(.flags[i]))
      .data[[paste0("mrgda_", names(.flags[i]))]] = paste0(names(.flags[i]), " flag missing")
    }

  }

  if (length(miss_flags) > 0) {
    cli::cli_h3("Flags not found in spec")
    cli::cli_ul(miss_flags)
  }

  list_return <- list()

  list_return[["data"]] <- .data
  list_return[["flags"]] <- .flags

  return(list_return)

}