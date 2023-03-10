gather_flags <- function(.data, .spec){

  recognized_flags <-
    system.file("package-data", "recognized-flags.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    dplyr::select(-Description) %>%
    dplyr::rename(name = `Flag name`, default = `Default Column`) %>%
    suppressMessages()

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags$name] %>%
    purrr::set_names(recognized_flags$name)

  # Check if all flags are NULL
  .flags_bin <- purrr::map(.flags, ~ is.null(.x))
  if (all(.flags_bin == TRUE)) {
    .any_mrgda_specific <- FALSE
    cli::cli_alert_info("No mrgda specific flags found in spec file")
  } else {
    .any_mrgda_specific <- TRUE
  }

  for (flag.i in names(.flags[unlist(.flags_bin)])) {

    defaults.i <-
      unlist(
        strsplit(recognized_flags$default[recognized_flags$name == flag.i], "_")
      )

    if (all(is.na(defaults.i))) {
      rm(defaults.i)
      next
    }

    for (default.i in defaults.i) {

      if (!is.null(.data[[default.i]])) {
        .flags[flag.i] <- default.i
      }

      rm(default.i)

    }

    rm(defaults.i)

  }


  list_return <- list()

  list_return[["data"]] <- .data
  list_return[["flags"]] <- .flags
  list_return[["any_mrgda_specific"]] <- .any_mrgda_specific

  return(list_return)

}
