#' Execute Differences Between Data Frames
#'
#' This function executes the comparison of two data frames, generates
#' the differences, and writes them to specified output files.
#'
#' @param .base_df A data frame that serves as the base for comparison.
#' @param .compare_df A data frame to compare against the base data frame.
#' @param .output_dir A string representing the directory where the output difference files will be stored.
#' @param .id_col A string representing the column name to be used for subject-based differences.
#' @param .header A string representing a box header.
#'
#' @return No return value, but this function will write files to the specified output directory
#'         containing the differences between the input data frames.
#'
#' @keywords internal
execute_data_diffs <- function(.base_df, .compare_df, .output_dir, .id_col = "ID", .header = ""){

  if (!dir.exists(.output_dir)) {
    stop(.output_dir, " does not exist")
  }

  # Diffs across entire data ------------------------------------------------
  full_diff <- diffdf::diffdf(
    base = .base_df,
    compare = .compare_df,
    file = file.path(.output_dir, "data-diff.txt"),
    suppress_warnings = TRUE
  )
  # cli::cli_alert_success(glue::glue("File written: {file.path(.output_dir, 'data-diff.txt')}"))

  if (length(full_diff) == 0) {
    cli::cli_alert_info("No diffs since last version found")
    return(invisible(NULL))
  }

  cli::cli_alert_info("Diffs since last version:")

  print_diffs <- tibble::tibble(
    name = "N Rows Diff (new - prev)",
    value = as.character(nrow(.compare_df) - nrow(.base_df))
  )

  if (!is.null(full_diff$ExtColsBase)) {

    print_diffs <- dplyr::bind_rows(
      print_diffs,
      tibble::tibble(
        name = "New Columns",
        value = paste(full_diff$ExtColsBase$COLUMNS, collapse = ", ")
      )
    )

  }

  if (!is.null(full_diff$ExtColsComp)) {

    print_diffs <- dplyr::bind_rows(
      print_diffs,
      tibble::tibble(
        name = "Removed Columns",
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

  print(
    cli::boxx(
      header = .header,
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

  # Diffs by id -------------------------------------------------------------
  datas_have_id <- (.id_col %in% names(.base_df)) & (.id_col %in% names(.compare_df))

  if (datas_have_id) {

    base_sl <-
      distinct_subject_columns(.base_df, .subject_col = .id_col) %>%
      dplyr::arrange(get(.id_col))

    compare_sl <-
      distinct_subject_columns(.compare_df, .subject_col = .id_col) %>%
      dplyr::arrange(get(.id_col))

    sl_equal <-  dplyr::all_equal(base_sl, compare_sl, convert = TRUE)

    if (inherits(sl_equal, "character")) {

      diffdf::diffdf(
        base = base_sl,
        compare = compare_sl,
        file = file.path(.output_dir, "subject-columns-diff.txt"),
        suppress_warnings = TRUE
      )

    }
    # cli::cli_alert_success(glue::glue("File written: {file.path(.output_dir, 'subject-columns-diff.txt')}"))

    id_diffs <- list()
    ids = unique(.base_df[[.id_col]])
    pb <- progress::progress_bar$new(total = length(ids))

    for (id.i in ids) {

      base_df.i <- .base_df[.base_df[[.id_col]] == id.i, ]
      compare_df.i <- .compare_df[.compare_df[[.id_col]] == id.i, ]

      equal.i <- dplyr::all_equal(base_df.i, compare_df.i, convert = TRUE)

      if (inherits(equal.i, "character")) {
        id_diffs[[paste0(.id_col, ":", id.i)]] <-
          diffdf::diffdf(
            base = base_df.i,
            compare = compare_df.i,
            suppress_warnings = TRUE
          )
      }

      pb$tick()

    }

    if (length(id_diffs) > 0) {

      id_diffs_out <- purrr::map_dfr(id_diffs, diffdf_value_changes_to_df, .id = .id_col)

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
