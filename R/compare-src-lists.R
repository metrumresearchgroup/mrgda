#' Compare Two Source Lists
#'
#' Compares two source lists (typically from [read_src_dir()]) and returns
#' a summary of differences in rows, columns, subjects, and date ranges per domain.
#'
#' @param .src_list1 First source list (the "base" for comparison).
#' @param .src_list2 Second source list (the "compare" target).
#' @param .subject_col Subject ID column name. Defaults to `"USUBJID"`.
#'
#' @return A tibble with one row per domain containing:
#' - `Domain`: Domain name
#' - `Status`: One of `"identical"`, `"modified"`, `"added"`, or `"removed"`
#' - `Rows`, `Cols`, `Subjects`, `Row/Subj`: Count comparisons
#' - `Date Min`, `Date Max`: Date range comparisons
#' - `Date Col`: Name of the DTC column used
#'
#' Status meanings:
#' - `"identical"`: Data frames are equal (ignoring attributes/class)
#' - `"modified"`: Data differs (even if summary stats match)
#' - `"added"`: Domain only in `.src_list2`
#' - `"removed"`: Domain only in `.src_list1`
#'
#' For `"modified"` domains, values show `"X -> Y"` for changes or
#' `"X (identical)"` when unchanged.
#'
#' @seealso [src_list_summary()] for summarizing a single source list
#'
#' @examples
#' \dontrun{
#' src1 <- read_src_dir(path1, .file_types = "xpt")
#' src2 <- read_src_dir(path2, .file_types = "xpt")
#' compare_src_lists(src1, src2)
#' }
#'
#' @export
compare_src_lists <- function(.src_list1, .src_list2, .subject_col = "USUBJID") {
  stopifnot(
    "`.src_list1` must be a list" = is.list(.src_list1),
    "`.src_list2` must be a list" = is.list(.src_list2),
    "`.subject_col` must be a single character string" = is.character(.subject_col) && length(.subject_col) == 1
  )

  sum1 <- src_list_summary(.src_list1, .subject_col)
  sum2 <- src_list_summary(.src_list2, .subject_col)

  all_domains <- union(sum1$Domain, sum2$Domain) %>% sort()

  purrr::map_dfr(all_domains, function(domain) {
    in1 <- domain %in% sum1$Domain
    in2 <- domain %in% sum2$Domain

    status <- dplyr::case_when(
      !in1 ~ "added",
      !in2 ~ "removed",
      is_data_identical(.src_list1[[domain]], .src_list2[[domain]]) ~ "identical",
      TRUE ~ "modified"
    )

    if (status != "modified") {
      return(tibble::tibble(
        Domain = domain, Status = status,
        Rows = status, Cols = status, Subjects = status, `Row/Subj` = status,
        `Date Min` = status, `Date Max` = status, `Date Col` = status
      ))
    }

    s1 <- dplyr::filter(sum1, .data$Domain == domain)
    s2 <- dplyr::filter(sum2, .data$Domain == domain)

    tibble::tibble(
      Domain = domain,
      Status = status,
      Rows = fmt_diff(s1$Rows, s2$Rows),
      Cols = fmt_diff(s1$Cols, s2$Cols),
      Subjects = fmt_diff(s1$Subjects, s2$Subjects),
      `Row/Subj` = fmt_diff(s1$`Row/Subj`, s2$`Row/Subj`),
      `Date Min` = fmt_diff(s1$`Date Min`, s2$`Date Min`),
      `Date Max` = fmt_diff(s1$`Date Max`, s2$`Date Max`),
      `Date Col` = dplyr::coalesce(s2$`Date Col`, s1$`Date Col`)
    )
  })
}


#' Check if two data frames are identical (ignoring attributes/class)
#' @noRd
is_data_identical <- function(df1, df2) {
  isTRUE(all.equal(df1, df2, check.attributes = FALSE, check.class = FALSE))
}


#' Format difference between two values
#' @noRd
fmt_diff <- function(v1, v2) {
  f1 <- fmt_num(v1)
  f2 <- fmt_num(v2)

  dplyr::case_when(
    is.na(v1) & is.na(v2) ~ NA_character_,
    is.na(v1) ~ paste0(f2, " (new)"),
    is.na(v2) ~ paste0(f1, " (removed)"),
    v1 == v2 ~ paste0(f1, " (identical)"),
    TRUE ~ paste0(f1, " -> ", f2)
  )
}


#' Format number with comma separators
#' @noRd
fmt_num <- function(x) {
 if (!is.numeric(x)) return(x)
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}
