gather_flags <- function(.data, .spec){

  recognized_flags <- tibble::tribble(
    ~name, ~default,
    "id", "ID",
    "dv", "DV",
    "amt", "AMT",
    "study", "STUDYID_STUDY",
    "primary_keys", NA_character_,
    "time", "TAFD_TIME",
    "bl_cat_cov", NA_character_,
    "bl_cont_cov", NA_character_,
    "tv_cat_cov", NA_character_,
    "tv_cont_cov", NA_character_,
    "num", "NUM",
    "mdv", "MDV",
    "evid", "EVID",
    "dvid", "DVID",
    "blq", "BQL_BLQ",
    "occ", "OCC"
  )

  .flags <- yspec::pull_meta(.spec, "flags")[recognized_flags$name] %>%
    purrr::set_names(recognized_flags$name)

  # Check if all flags are NULL
  .flags_bin <- purrr::map(.flags, ~ is.null(.x))
  if (all(.flags_bin == TRUE)) {
    message("No flags found in spec file")
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

  return(list_return)

}
