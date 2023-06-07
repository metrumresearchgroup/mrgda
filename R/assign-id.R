#' Assign Subject IDs (creates an ID column)
#'
#' @description
#' Upon first use, an ID column will be mutated to the given data set according
#' to the specified subject column.
#'
#' For each subsequent use, IDs will be assigned to match the previous derived data
#' set. If new subjects are found in the data, they will be assigned a
#' unique value not present in the previous derived data (starting at the maximum
#' of the previous IDs plus 1).
#'
#' @param .data A data.frame to have ID column mutated onto.
#' @param .previously_derived_path A string containing the file path of the previous derived data set
#' @param .subject_col A character string containing the column name of the subject identifying column in the data
#'
#' @examples
#' Theoph2 <-
#'   Theoph %>%
#'   dplyr::mutate(USUBJID = paste0("Subject-", Subject)) %>%
#'   dplyr::select(-Subject)
#'
#' df_with_id <-
#'   assign_id(
#'     Theoph2,
#'     .subject_col = "USUBJID"
#'   )
#'
#' @export
assign_id <- function(.data, .previously_derived_path = NULL, .subject_col = "USUBJID") {

  # Check if the subject column does exist and ID doesn't exist in .data
  if (!is.null(.data[["ID"]])) {
    stop("Data already contains ID")
  }

  if (is.null(.data[[.subject_col]])) {
    stop(paste0(.subject_col, " not found in data"))
  }

  # If a previously derived data path is provided -
  ## 1) Read in to data.frame
  ## 2) Check if both ID and .subject_col exist in the previous data
  ## 3) Select only ID and .subject_col columns and run distinct

  if (!is.null(.previously_derived_path)) {
    previously_derived <-
      data.table::fread(.previously_derived_path) %>%
      suppressMessages()

    if (is.null(previously_derived[["ID"]])) {
      stop("ID column not found in previous data")
    }

    if (is.null(previously_derived[[.subject_col]])) {
      stop(paste0(.subject_col, " not found in previous data"))
    }

    prev_id_lookup <-
      previously_derived %>%
      dplyr::select(dplyr::all_of(c("ID", .subject_col))) %>%
      dplyr::distinct()

  } else {
    # If no previous data available, make empty tibble with columns ID and .subject_col
    prev_id_lookup <-
      .data %>%
      dplyr::mutate(ID = NA_real_) %>%
      dplyr::select(dplyr::all_of(c("ID", .subject_col))) %>%
      dplyr::slice(0)
  }

  # If previous derived data has ID, join onto .data
  # For subjects without ID, create a new unique ID for them
  # If some subjects had previous ID, new ID's started at 1 plus the max ID in the previous data

  data_join_id_lookup <-
    .data %>%
    dplyr::left_join(prev_id_lookup) %>%
    dplyr::mutate(
      mrgda_MAX_ID = ifelse(all(is.na(ID)), 0, max(ID, na.rm = TRUE)),
      mrgda_SUBJ_NEED_ID = ifelse(is.na(ID), !!sym(.subject_col), NA),
      ID = ifelse(
        is.na(ID),
        as.numeric(forcats::fct_inorder(mrgda_SUBJ_NEED_ID)) + mrgda_MAX_ID,
        ID)
    ) %>%
    dplyr::select(-tidyr::starts_with("mrgda_"))

  print(
    cli::boxx(
      header = "ID Summary",
      label = c(
        paste0("Number of subjects previously assigned: ", nrow(prev_id_lookup)),
        paste0("Number of subjects newly assigned: ", nrow(data_join_id_lookup) - nrow(prev_id_lookup))
      )
    )
  )

  # Join on new ID lookup to the original data
  .data_w_id <-
    .data %>%
    dplyr::left_join(data_join_id_lookup) %>%
    suppressMessages()

  # Perform final checks on the data
  stopifnot(ncol(.data_w_id) == ncol(.data) + 1)
  stopifnot(nrow(.data_w_id) == nrow(.data))
  stopifnot(!is.null(.data_w_id$ID))
  stopifnot(!anyNA(.data_w_id$ID))
  stopifnot(
    .data_w_id %>%
      dplyr::select(dplyr::all_of(c("ID", .subject_col))) %>%
      dplyr::distinct() %>%
      is_unique_by_subject(.df = ., .column = "ID")
  )

  return(.data_w_id)

}
