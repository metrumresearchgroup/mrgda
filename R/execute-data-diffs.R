#' Execute Differences Between Data Frames
#'
#' This function executes the comparison of two data frames, generates
#' the differences, and writes them to specified output files.
#'
#' @param .base_df A data frame that serves as the base for comparison.
#' @param .compare_df A data frame to compare against the base data frame.
#' @param .output_dir A string representing the directory where the output difference files will be stored.
#' @param .id_col A string representing the column name to be used for subject-based differences.
#'        Default is "ID".
#'
#' @return No return value, but this function will write files to the specified output directory
#'         containing the differences between the input data frames.
#'
#' @keywords internal
execute_data_diffs <- function(.base_df, .compare_df, .output_dir, .id_col = "ID"){

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

  cli::cli_alert_info("Diffs since last version:")

  if(!is.null(full_diff$NumDiff)){

    print(
      cli::boxx(
        header = 'Columns in common',
        padding = 0,
        col = 'blue',
        knitr::kable(
          x = full_diff$NumDiff,
          align = 'c',
          format = "simple"
        )
      )
    )
  }

  if(!is.null(full_diff$ExtColsBase)){

    print(
      cli::boxx(
        padding = 0,
        col = 'red',
        knitr::kable(
          x = tibble::tibble("Columns Dropped" = full_diff$ExtColsBase$COLUMNS),
          align = 'c',
          format = "simple"
        )
      )
    )
  }

  if(!is.null(full_diff$ExtColsComp)){

    print(
      cli::boxx(
        padding = 0,
        col = 'green',
        knitr::kable(
          x = tibble::tibble("Columns Added" = full_diff$ExtColsComp$COLUMNS),
          align = 'c',
          format = "simple"
        )
      )
    )

  }

  # Clear out NUM -----------------------------------------------------------
  .base_df$NUM <- NULL
  .compare_df$NUM <- NULL

  # Diffs by id -------------------------------------------------------------
  datas_have_id <- (.id_col %in% names(.base_df)) & (.id_col %in% names(.compare_df))

  if (datas_have_id) {

    diffdf::diffdf(
      base = distinct_subject_columns(.base_df, .subject_col = .id_col),
      compare = distinct_subject_columns(.compare_df, .subject_col = .id_col),
      file = file.path(.output_dir, "subject-columns-diff.txt"),
      suppress_warnings = TRUE
    )
    # cli::cli_alert_success(glue::glue("File written: {file.path(.output_dir, 'subject-columns-diff.txt')}"))

    id_diffs <- list()
    ids = unique(.base_df[[.id_col]])
    pb <- progress::progress_bar$new(total = length(ids))

    for (id.i in ids) {

      base_df.i <- .base_df[.base_df[[.id_col]] == id.i, ]
      compare_df.i <- .compare_df[.compare_df[[.id_col]] == id.i, ]

      equal.i <- all.equal(base_df.i, compare_df.i)

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

      id_diffs_out <- purrr::map_dfr(id_diffs, diffdf_to_df, .id = .id_col)

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
