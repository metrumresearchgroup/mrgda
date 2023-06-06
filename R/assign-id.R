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

  if (!is.null(.data[["ID"]])) {
    stop("Data already contains ID")
  }

  if (is.null(.data[[.subject_col]])) {
    stop("Subject column not found in data")
  }

  make_id <- function(.data, .subject_col, .to_add = 0, .new_id = FALSE){

    .data_with_id <-
      .data %>%
      dplyr::mutate(
        ID = as.numeric(forcats::fct_inorder(!!sym(.subject_col))) + .to_add
      )

    .count_id <- .data_with_id %>% dplyr::distinct(ID)

    if (.new_id) {
      print(
        cli::boxx(
          header = "New IDs Assigned",
          label = c(paste0("Number of subjects assigned: ", nrow(.count_id))))
      )
    }

    return(.data_with_id)
  }

  # Assigns IDs from scratch if no path
  if(is.null(.previously_derived_path)){
    return(make_id(.data, .subject_col))
  }

  # Assigns IDs from scratch if no previous file
  if(!file.exists(.previously_derived_path)){
    return(make_id(.data, .subject_col))
  }

  previous_data <- readr::read_csv(.previously_derived_path) %>% suppressMessages()

  if (is.null(previous_data[["ID"]])) {
    stop("ID column not found in previous data ")
  }

  if (is.null(previous_data[[.subject_col]])) {
    stop("Subject column not found in previous data ")
  }

  id_lookup <-
    previous_data %>%
    dplyr::select(dplyr::all_of(c("ID", .subject_col))) %>%
    dplyr::distinct()

  stopifnot(!anyNA(id_lookup))

  new_id_check <- !all(.data[[.subject_col]] %in% id_lookup[[.subject_col]])

  # New subjects coming into existing data set
  if (new_id_check) {

    missing_ids <-
      .data %>%
      dplyr::left_join(id_lookup) %>%
      dplyr::filter(is.na(ID)) %>%
      dplyr::distinct(!!sym(.subject_col)) %>%
      make_id(.data = .,
              .subject_col = .subject_col,
              .to_add = max(id_lookup$ID, na.rm = TRUE),
              .new_id = TRUE)

    id_lookup <- id_lookup %>% dplyr::bind_rows(missing_ids)

    stopifnot(is_unique_by_subject(id_lookup, .subject_col))
    stopifnot(is_unique_by_subject(id_lookup, "ID"))

  } else {
    print(
      cli::boxx(
        label = "No new IDs assigned")
    )
  }

  .data_w_id <-
    .data %>%
    dplyr::left_join(id_lookup) %>%
    suppressMessages()

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
