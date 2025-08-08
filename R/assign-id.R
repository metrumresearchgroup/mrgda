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

  # Check if data is grouped
  if (dplyr::is_grouped_df(.data)) {
    stop("Ungroup data before assign_id")
  }

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
      read_csv_dots(.previously_derived_path)

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


  # Helper to generate letter IDs: A, B, ..., Z, AA, BB, ...
  letter_ids <- function(n, start = 1) {
    # n: number of IDs to generate
    # start: starting index (1 = A, 2 = B, ...)
    ids <- character(n)
    alphabet <- LETTERS
    for (i in seq_len(n)) {
      idx <- start + i - 1
      if (idx <= 26) {
        ids[i] <- alphabet[idx]
      } else {
        # After Z, repeat letters: AA, BB, ...
        rep_count <- ((idx - 1) %/% 26)
        letter_idx <- ((idx - 1) %% 26) + 1
        ids[i] <- paste(rep(alphabet[letter_idx], rep_count + 1), collapse = "")
      }
    }
    ids
  }

  # Find max previous ID (as letter index)
  prev_ids <- prev_id_lookup$ID
  prev_ids_num <- function(ids) {
    # Convert letter IDs to numeric index: A=1, ..., Z=26, AA=27, BB=28, ...
    sapply(ids, function(x) {
      if (is.na(x)) return(NA_integer_)
      # Count how many times the first letter is repeated
      nchar(x) * (match(substr(x, 1, 1), LETTERS))
    })
  }
  max_prev_id <- suppressWarnings(max(prev_ids_num(prev_ids), na.rm = TRUE))
  if (is.infinite(max_prev_id)) max_prev_id <- 0

  # Assign IDs
  .data2 <- .data %>% dplyr::left_join(prev_id_lookup)
  need_id <- which(is.na(.data2$ID))
  n_new <- length(need_id)
  if (n_new > 0) {
    new_ids <- letter_ids(n_new, start = max_prev_id + 1)
    .data2$ID[need_id] <- new_ids
  }
  data_join_id_lookup <- .data2 %>%
    dplyr::select(dplyr::all_of(c("ID", .subject_col))) %>%
    dplyr::distinct() %>%
    suppressMessages()

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

  print(
    cli::boxx(
      header = "ID Summary",
      label = c(
        paste0("Number of subjects detected and assigned IDs: ", length(unique(.data_w_id$ID)))
      )
    )
  )

  return(.data_w_id)

}
