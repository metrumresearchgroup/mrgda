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
#'   \item{DOMAIN}{Domain name}
#'   \item{LIST1}{Logical indicating if domain is present in .src_list1}
#'   \item{LIST2}{Logical indicating if domain is present in .src_list2}
#'   \item{ROWS}{Difference in number of rows (list2 - list1)}
#'   \item{COLS}{Difference in number of columns (list2 - list1)}
#'   \item{SUBJ}{Difference in number of unique subjects (list2 - list1)}
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
  out <- dplyr::tibble(
    DOMAIN = character(),
    LIST1 = logical(),
    LIST2 = logical(),
    ROWS = integer(),
    COLS = integer(),
    SUBJ = integer()
  )

  for (domain.i in all_domains) {
    df1 <- .src_list1[[domain.i]]
    df2 <- .src_list2[[domain.i]]

    in_list1 <- !is.null(df1) && is.data.frame(df1)
    in_list2 <- !is.null(df2) && is.data.frame(df2)

    # Use NA for diffs if domain is missing from either list
    if (!in_list1 || !in_list2) {
      row_diff <- NA_integer_
      col_diff <- NA_integer_
      subj_diff <- NA_integer_
    } else {
      nrow1 <- nrow(df1)
      nrow2 <- nrow(df2)
      ncol1 <- ncol(df1)
      ncol2 <- ncol(df2)
      nsubj1 <- if (.subject_col %in% names(df1)) length(unique(df1[[.subject_col]])) else 0
      nsubj2 <- if (.subject_col %in% names(df2)) length(unique(df2[[.subject_col]])) else 0

      row_diff <- nrow2 - nrow1
      col_diff <- ncol2 - ncol1
      subj_diff <- nsubj2 - nsubj1
    }

    out <- dplyr::bind_rows(
      out,
      dplyr::tibble(
        DOMAIN = domain.i,
        LIST1 = in_list1,
        LIST2 = in_list2,
        ROWS = row_diff,
        COLS = col_diff,
        SUBJ = subj_diff
      )
    )
  }

  out
}
