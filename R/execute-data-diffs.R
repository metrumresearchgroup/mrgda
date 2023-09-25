#' Execute Differences Between Data Frames
#'
#' This function executes the comparison of two data frames, generates
#' the differences, and writes them to specified output files.
#'
#' @param .base_df A data frame that serves as the base for comparison.
#' @param .compare_df A data frame to compare against the base data frame.
#' @param .subject_col A string representing the column name to be used for subject-based differences.
#'        Set to `NULL` for data frames not containing an subject column.
#' @param .base_from_svn Logical. Was base df exported from svn?
#'
#' @return No return value, but this function will write files to the specified output directory
#'         containing the differences between the input data frames.
#'
#' @export
execute_data_diffs <- function(.base_df, .compare_df, .subject_col, .base_from_svn = FALSE){

  # Exit if no names in common ----------------------------------------------
  names_in_common <- dplyr::intersect(names(.base_df), names(.compare_df))

  if (length(names_in_common) == 0) {
    stop("The base and compare data frames do not have any columns to compare")
  }

  out <- list()

  # Initialize to empty data frames
  out$diffs <- tibble::tribble(~name, ~value)

  out$subject_diffs <-
    tibble::tribble(~SUBJECT, ~VARIABLE, ~BASE, ~COMPARE,~`N Occurrences`)

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

  if (length(full_diff) == 0) {
    cli::cli_alert_info("No diffs since last version found")
    return(out)
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

  print_diffs <- dplyr::bind_rows(
    print_diffs,
    tibble::tibble(
      name = "Compare data from",
      value = dplyr::if_else(.base_from_svn, "svn", "local")
    )
  )



  if(is.null(.subject_col)){

    out$diffs <- print_diffs
    out$subject_diffs <- NULL
    out$value_diffs <- diffdf_value_changes_to_df(full_diff)

    print(
      cli::boxx(
        padding = 0,
        knitr::kable(
          x = out$diffs,
          align = 'c',
          format = "simple"
        )
      )
    )
    return(out)
  }

  datas_have_id <- (.subject_col %in% names(.base_df)) && (.subject_col %in% names(.compare_df))

  if(!datas_have_id){
    stop(glue::glue("The specified `.subject_col` ({.subject_col}) is not present in one or both of the data frames"))
  }

  n_id_diff <- length(unique(.compare_df[[.subject_col]])) - length(unique(.base_df[[.subject_col]]))

  n_id_diff_msg <- dplyr::case_when(
    n_id_diff == 0 ~ "No change in N IDs",
    n_id_diff < 0 ~ paste0(gsub("-", "", as.character(n_id_diff), fixed=TRUE), " ID(s) removed"),
    n_id_diff > 0 ~ paste0(n_id_diff, " ID(s) added")
  )

  print_diffs <- dplyr::bind_rows(
    print_diffs,
    tibble::tibble(
      name = "N IDs Diff",
      value = n_id_diff_msg
    )
  )

  out$diffs <- print_diffs


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
  .base_df[["NUM"]] <- NULL
  .compare_df[["NUM"]] <- NULL
  names_in_common <- names_in_common[names_in_common != "NUM"]

  # Diffs by id -------------------------------------------------------------
  id_diffs <- list()
  ids <- unique(dplyr::intersect(.base_df[[.subject_col]], .compare_df[[.subject_col]]))
  cli::cli_progress_bar("Checking data differences", total = length(ids))

  for (id.i in ids) {

    cli::cli_progress_update()

    base_df.i <-
      .base_df[.base_df[[.subject_col]] == id.i, ] %>%
      dplyr::select(dplyr::all_of(names_in_common))

    compare_df.i <-
      .compare_df[.compare_df[[.subject_col]] == id.i, ] %>%
      dplyr::select(dplyr::all_of(names_in_common))

    # If they are identical (we do not care if attributes are different),
    # skip the diffdf check to save time
    all_equal.i <- all.equal(
      target = base_df.i,
      current = compare_df.i,
      check.attributes = FALSE
    )

    if (!isTRUE(all_equal.i)) {

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

    }

    rm(all_equal.i)

  }

  cli::cli_progress_done()

  if (length(id_diffs) > 0) {

    id_diffs_out <- purrr::map_dfr(id_diffs, diffdf_value_changes_to_df, .id = .subject_col)

    if (nrow(id_diffs_out) > 0) {

      out$subject_diffs <-
        id_diffs_out %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::group_by(!!sym(.subject_col), VARIABLE, BASE, COMPARE) %>%
        dplyr::summarise(`N Occurrences` = dplyr::n()) %>%
        suppressMessages() %>%
        dplyr::ungroup()
    }
  }

  return(out)
}
