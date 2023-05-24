#' Subject ID Assignment Tracking
#'
#' @description
#' Upon first use, an ID column will be mutated to the given data set according
#' to the specified subject column. In addition, an ID lookup csv file will be
#' created containing the assigned ID for each subject.
#'
#' For each future use, ID's will be assigned according to the lookup csv file. If
#' new subjects are added to the data, the lookup file will be modified to include
#' the new subjects to assign them each a unique identifier.
#'
#' @param .data A data.frame to have ID column mutated onto.
#' @param .lookup_path A string containing the file path of the ID lookup csv file
#' @param .subject_col A character string containing the column name of the subject identifying column in the data
#'
#' @details
#' This function creates a lookup for subject IDs. If an ID column already exists in the data,
#' or the specified subject column does not exist the function will stop.
#' If a lookup file does not exist at the specified path, the function
#' will create a new lookup and write it to the provided path. If a lookup file does exist,
#' the function will assign ID to the data according to the lookup.
#'
#' If subjects exist in the data but not in the lookup, they will be added to the lookup
#' and assigned an unique ID value.
#'
#' @examples
#' Theoph2 <-
#'   Theoph %>%
#'   dplyr::mutate(USUBJID = paste0("Subject-", Subject)) %>%
#'   dplyr::select(-Subject)
#'
#' df_with_id <-
#'   id_lookup_tracker(
#'     Theoph2,
#'     .lookup_path = paste0(tempfile(), ".csv"),
#'     .subject_col = "USUBJID"
#'   )
#'
#' @return A data frame with an additional ID column, which contains the numeric IDs for each subject in the subject column.
#'
#' @export
id_lookup_tracker <- function(.data, .lookup_path, .subject_col = "USUBJID") {

  if (!is.null(.data[["ID"]])) {
    stop("Data already contains ID")
  }

  if (is.null(.data[[.subject_col]])) {
    stop("Subject column not found in data")
  }

  if(!file.exists(.lookup_path)) {

    id_lookup <-
      .data %>%
      dplyr::mutate(
        ID =  as.numeric(forcats::fct_inorder(!!sym(.subject_col)))
      ) %>%
      dplyr::select(c("ID", .subject_col)) %>%
      dplyr::distinct()

    stopifnot(nrow(id_lookup %>% dplyr::count(USUBJID) %>% dplyr::filter(n > 1)) == 0)

    readr::write_csv(x = id_lookup, file = .lookup_path)

    print(
      cli::boxx(
        header = "New ID Lookup Created",
        label = c(paste0("Number of subjects in lookup: ", nrow(id_lookup)),
                  paste0("Lookup file written to: ", .lookup_path)))
    )

  }

  add_id <-
    .data %>%
    dplyr::left_join(readr::read_csv(.lookup_path)) %>%
    suppressMessages()

  stopifnot(ncol(add_id) == ncol(.data) + 1)

  # New subjects coming into existing data set
  if (anyNA(add_id$ID)) {

    missing_ids <-
      add_id %>%
      dplyr::filter(is.na(ID)) %>%
      dplyr::distinct(!!sym(.subject_col)) %>%
      dplyr::mutate(
        ID =  as.numeric(forcats::fct_inorder(!!sym(.subject_col))) + max(add_id$ID, na.rm = TRUE)
      )

    add_lookup <-
      readr::read_csv(.lookup_path) %>%
      dplyr::bind_rows(missing_ids) %>%
      suppressMessages()

    stopifnot(nrow(add_lookup %>% dplyr::count(USUBJID) %>% dplyr::filter(n > 1)) == 0)

    readr::write_csv(add_lookup, file = .lookup_path)

    add_id <-
      .data %>%
      dplyr::left_join(readr::read_csv(.lookup_path)) %>%
      suppressMessages()

    id_subj_combo <-
      missing_ids %>%
      dplyr::mutate(
        SUBJCOL = paste0(ID, " - ", !!sym(.subject_col))
      )

    print(
      cli::boxx(
        header = "New Subjects Added to ID Lookup",
        label = c(paste0("Number of new subjects added to lookup: ", nrow(missing_ids)),
                  paste0("New ID: ", id_subj_combo$SUBJCOL)))
    )

  }

  return(add_id)

}
