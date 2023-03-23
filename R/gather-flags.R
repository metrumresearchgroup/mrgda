#' @keywords internal
gather_flags <- function(.data, .spec, .verbose = FALSE){

  list_return <- list()

  recognized_flags <-
    system.file("package-data", "recognized-flags.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    #dplyr::select(-Description) %>%
    dplyr::rename(name = `Flag name`, default = `Default Column`) %>%
    suppressMessages()

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags$name] %>%
    purrr::set_names(recognized_flags$name)

  manual_flags <- .flags

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


  flags_check <- purrr::map(.flags, ~is.null(.x))

  missing_flags <-
    recognized_flags %>%
    dplyr::filter(name %in% names(flags_check[unlist(flags_check)]))




  if(.verbose){

    if (length(missing_flags) > 0) {
      cli::cli_alert_info("Undefined mrgda flags in spec file:")
      cli::cli_bullets(
        missing_flags %>%
          dplyr::transmute(desc = paste0(name, " (",Description, ")")) %>%
          dplyr::pull(desc) %>%
          purrr::set_names(rep("*", length(.)))
      )
    }

  }

  list_return[["data"]] <- .data
  list_return[["flags"]] <- .flags
  list_return[["any_mrgda_specific"]] <- .any_mrgda_specific
  list_return[["recognized_flags"]] <- recognized_flags
  list_return[["manual_flags"]] <- manual_flags
  list_return[["missing_flags"]] <- missing_flags

  return(list_return)

}
