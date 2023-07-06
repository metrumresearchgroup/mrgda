#' Execute Differences Between Data Frames
#'
#' This function executes the comparison of two data frames, generates
#' the differences, and writes them to specified output files.
#'
#' @param .base_df A data frame that serves as the base for comparison.
#' @param .compare_df A data frame to compare against the base data frame.
#' @param .output_dir A string representing the directory where the output difference files will be stored.
#' @param .id_col A string representing the column name to be used for subject-based differences.
#' @param .base_from_svn Logical. Was base df exported from svn?
#'
#' @return No return value, but this function will write files to the specified output directory
#'         containing the differences between the input data frames.
#'
#' @keywords internal
execute_data_diffs <- function(.base_df, .compare_df, .output_dir, .id_col = "ID", .base_from_svn = FALSE){

  if (!dir.exists(.output_dir)) {
    stop(.output_dir, " does not exist")
  }

  # Diffs across entire data ------------------------------------------------
  full_diff <- suppressMessages(
    diffdf::diffdf(
      base = .base_df,
      compare = .compare_df,
      suppress_warnings = TRUE,
      strict_numeric = FALSE,
      strict_factor = FALSE
    )
  )
  # cli::cli_alert_success(glue::glue("File written: {file.path(.output_dir, 'data-diff.txt')}"))

  if (length(full_diff) == 0) {
    cli::cli_alert_info("No diffs since last version found")

    readr::write_csv(
      tibble::tribble(~name, ~value),
      file.path(.output_dir, "diffs.csv")
    )

    return(invisible(NULL))
  }

  cli::cli_alert_info("Diffs since last version:")

  n_row_diff <- nrow(.compare_df) - nrow(.base_df)

  n_row_diff_msg <- dplyr::case_when(
    n_row_diff == 0 ~ "No change in N rows",
    n_row_diff < 0 ~ paste0(gsub("-", "", as.character(n_row_diff), fixed=TRUE), " row(s) removed"),
    n_row_diff > 0 ~ paste0(n_row_diff, " row(s) added")
  )

  print_diffs <- tibble::tibble(
    name = "N Rows Diff",
    value = n_row_diff_msg
  )

  if (!is.null(full_diff$ExtColsBase)) {

    print_diffs <- dplyr::bind_rows(
      print_diffs,
      tibble::tibble(
        name = "Removed Columns",
        value = paste(full_diff$ExtColsBase$COLUMNS, collapse = ", ")
      )
    )

  }

  if (!is.null(full_diff$ExtColsComp)) {

    print_diffs <- dplyr::bind_rows(
      print_diffs,
      tibble::tibble(
        name = "New Columns",
        value = paste(full_diff$ExtColsComp$COLUMNS, collapse = ", ")
      )
    )

  }


  if (!is.null(full_diff$NumDiff)) {

    print_diffs <- dplyr::bind_rows(
      print_diffs,
      full_diff$NumDiff %>%
        dplyr::transmute(name = Variable, value = paste0("N Diffs: ", `No of Differences`))
    )
  }


  datas_have_id <- (.id_col %in% names(.base_df)) & (.id_col %in% names(.compare_df))

  if(datas_have_id){

    n_id_diff <- length(unique(.compare_df[[.id_col]])) - length(unique(.base_df[[.id_col]]))

    n_id_diff_msg <- dplyr::case_when(
      n_id_diff == 0 ~ "No change in N IDs",
      n_id_diff < 0 ~ paste0(gsub("-", "", as.character(n_id_diff), fixed=TRUE), " ID(s) removed"),
      n_id_diff > 0 ~ paste0(n_id_diff, " ID(s) added")
    )

    print_diffs <-
      print_diffs <- dplyr::bind_rows(
        print_diffs,
        tibble::tibble(
          name = "N IDs Diff",
          value = n_id_diff_msg
        )
      )
  }

  print_diffs <- dplyr::bind_rows(
    print_diffs,
    tibble::tibble(
      name = "Compare data from",
      value = dplyr::if_else(.base_from_svn, "svn", "local")
    )
  )

  readr::write_csv(print_diffs, file.path(.output_dir, "diffs.csv"))

  print(
    cli::boxx(
      padding = 0,
      knitr::kable(
        x = print_diffs,
        align = 'c',
        format = "simple"
      )
    )
  )


  # Clear out NUM -----------------------------------------------------------
  .base_df$NUM <- NULL
  .compare_df$NUM <- NULL

  # Only select names in common ---------------------------------------------
  names_in_common <- dplyr::intersect(names(.base_df), names(.compare_df))

  if (length(names_in_common) == 0) {
    return(invisible(NULL))
  }


  # Diffs by id -------------------------------------------------------------

  if (datas_have_id) {

    id_diffs <- list()
    ids <- unique(dplyr::intersect(.base_df[[.id_col]], .compare_df[[.id_col]]))
    pb <- progress::progress_bar$new(total = length(ids))

    for (id.i in ids) {

      base_df.i <-
        .base_df[.base_df[[.id_col]] == id.i, ] %>%
        dplyr::select(dplyr::all_of(names_in_common))

      compare_df.i <-
        .compare_df[.compare_df[[.id_col]] == id.i, ] %>%
        dplyr::select(dplyr::all_of(names_in_common))

      diff.i <-
        suppressMessages(
          diffdf::diffdf(
            base = base_df.i,
            compare = compare_df.i,
            suppress_warnings = TRUE,
            strict_numeric = FALSE,
            strict_factor = FALSE
          )
        )

      if (length(diff.i) > 0) {
        id_diffs[[as.character(id.i)]] <- diff.i
      }

      rm(diff.i)

      pb$tick()

    }

    if (length(id_diffs) > 0) {

      id_diffs_out <- purrr::map_dfr(id_diffs, diffdf_value_changes_to_df, .id = .id_col)

      if (nrow(id_diffs_out) == 0) {

        readr::write_csv(
          tibble::tribble(~ID, ~VARIABLE, ~BASE, ~COMPARE,~`N Occurrences`) %>% dplyr::rename(!!sym(.id_col) := "ID"),
          file.path(.output_dir, 'id-diffs.csv')
        )

        return(invisible(NULL))
      }

      id_diffs_out <-
        id_diffs_out %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::group_by(!!sym(.id_col), VARIABLE, BASE, COMPARE) %>%
        dplyr::summarise(`N Occurrences` = dplyr::n()) %>%
        suppressMessages() %>%
        dplyr::ungroup()

      data.table::fwrite(
        x = id_diffs_out,
        file = file.path(.output_dir, 'id-diffs.csv'),
        sep = ",",
        quote = FALSE,
        row.names = FALSE,
        na = "."
      )

      # cli::cli_alert_success(glue::glue("File written: {file.path(.output_dir, 'id-diffs.csv')}"))

    }
  }
}
