#' Compare two source lists
#'
#' @description
#' Compare two source lists (typically created by `read_src_dir()`) and return
#' a data frame summarizing differences in rows, columns, and subjects per domain.
#'
#' @param .src_list1 First source list to compare (considered the "base").
#' @param .src_list2 Second source list to compare (considered the "compare").
#' @param .subject_col Character string specifying the subject ID column name.
#'   Defaults to "USUBJID".
#'
#' @return A tibble with one row per domain and columns:
#' \describe{
#'   \item{Domain}{Domain name}
#'   \item{Status}{Comparison status: "identical", "modified", "added", or "removed"}
#'   \item{Rows 1, Rows 2}{Number of rows in each list}
#'   \item{Cols 1, Cols 2}{Number of columns in each list}
#'   \item{Subjects 1, Subjects 2}{Number of unique subjects in each list}
#'   \item{Rec/Subj 1, Rec/Subj 2}{Records per subject (rows/subjects) in each list}
#'   \item{Min Date 1, Max Date 1}{Date range from DTC columns in list1}
#'   \item{Min Date 2, Max Date 2}{Date range from DTC columns in list2}
#' }
#'
#' @examples
#' \dontrun{
#' src_list1 <- read_src_dir(path1, .file_types = "xpt")
#' src_list2 <- read_src_dir(path2, .file_types = "xpt")
#' compare_src_lists(src_list1, src_list2)
#' }
#'
#' @export
compare_src_lists <- function(.src_list1,
                              .src_list2,
                              .subject_col = "USUBJID") {

  # Input validation
  if (!inherits(.src_list1, "list")) {
    stop("`.src_list1` must be a list (preferably the output of `mrgda::read_src_dir()`)")
  }
  if (!inherits(.src_list2, "list")) {
    stop("`.src_list2` must be a list (preferably the output of `mrgda::read_src_dir()`)")
  }
  if (!is.character(.subject_col) || length(.subject_col) != 1) {
    stop("`.subject_col` must be a single character string")
  }

  # Extract metadata elements
  meta_elements <- c("mrgda_labels", "mrgda_src_meta")

  # Get domain names (excluding metadata)
  domains1 <- setdiff(names(.src_list1), meta_elements)
  domains2 <- setdiff(names(.src_list2), meta_elements)

  # All unique domains
  all_domains <- sort(unique(c(domains1, domains2)))

  # Build comparison data frame
  results <- lapply(all_domains, function(domain.i) {
    df1 <- .src_list1[[domain.i]]
    df2 <- .src_list2[[domain.i]]

    in_list1 <- !is.null(df1) && is.data.frame(df1)
    in_list2 <- !is.null(df2) && is.data.frame(df2)

    # Get counts for list1
    if (in_list1) {
      nrow1 <- nrow(df1)
      ncol1 <- ncol(df1)
      nsubj1 <- if (.subject_col %in% names(df1)) length(unique(df1[[.subject_col]])) else NA_integer_
      rps1 <- if (!is.na(nsubj1) && nsubj1 > 0) round(nrow1 / nsubj1, 1) else NA_real_
      dtc1 <- get_dtc_range(df1)
    } else {
      nrow1 <- NA_integer_
      ncol1 <- NA_integer_
      nsubj1 <- NA_integer_
      rps1 <- NA_real_
      dtc1 <- list(min = NA_character_, max = NA_character_)
    }

    # Get counts for list2
    if (in_list2) {
      nrow2 <- nrow(df2)
      ncol2 <- ncol(df2)
      nsubj2 <- if (.subject_col %in% names(df2)) length(unique(df2[[.subject_col]])) else NA_integer_
      rps2 <- if (!is.na(nsubj2) && nsubj2 > 0) round(nrow2 / nsubj2, 1) else NA_real_
      dtc2 <- get_dtc_range(df2)
    } else {
      nrow2 <- NA_integer_
      ncol2 <- NA_integer_
      nsubj2 <- NA_integer_
      rps2 <- NA_real_
      dtc2 <- list(min = NA_character_, max = NA_character_)
    }

    # Determine status
    status <- dplyr::case_when(
      !in_list1 & in_list2 ~ "added",
      in_list1 & !in_list2 ~ "removed",
      nrow1 == nrow2 & ncol1 == ncol2 &
        (is.na(nsubj1) & is.na(nsubj2) | (!is.na(nsubj1) & !is.na(nsubj2) & nsubj1 == nsubj2)) ~ "identical",
      TRUE ~ "modified"
    )

    dplyr::tibble(
      Domain = domain.i,
      Status = status,
      `Rows 1` = nrow1,
      `Rows 2` = nrow2,
      `Cols 1` = ncol1,
      `Cols 2` = ncol2,
      `Subjects 1` = nsubj1,
      `Subjects 2` = nsubj2,
      `Rec/Subj 1` = rps1,
      `Rec/Subj 2` = rps2,
      `Min Date 1` = dtc1$min,
      `Max Date 1` = dtc1$max,
      `Min Date 2` = dtc2$min,
      `Max Date 2` = dtc2$max
    )
  })

  dplyr::bind_rows(results)
}


#' Get date range from DTC columns
#'
#' @param df A data frame
#' @return Named list with min and max dates, or NAs if none found
#' @keywords internal
#' @noRd
get_dtc_range <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(min = NA_character_, max = NA_character_))
  }

  dtc_cols <- grep("DTC$", names(df), value = TRUE)

  if (length(dtc_cols) == 0) {
    return(list(min = NA_character_, max = NA_character_))
  }

  # Extract date portion from all DTC columns
  all_dates <- unlist(lapply(dtc_cols, function(col) {
    vals <- as.character(df[[col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    # Only keep values that look like full dates (YYYY-MM-DD)
    vals <- vals[nchar(vals) >= 10]
    substr(vals, 1, 10)
  }))

  if (length(all_dates) == 0) {
    return(list(min = NA_character_, max = NA_character_))
  }

  # Parse dates, ignoring failures
  parsed <- tryCatch(
    as.Date(all_dates),
    error = function(e) as.Date(NA)
  )
  parsed <- parsed[!is.na(parsed)]

  if (length(parsed) == 0) {
    return(list(min = NA_character_, max = NA_character_))
  }

  list(
    min = as.character(min(parsed)),
    max = as.character(max(parsed))
  )
}
