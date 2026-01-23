#' Compare two source lists
#'
#' @description
#' Compare two source lists (typically created by `read_src_dir()`) and return
#' a summary of differences including domains, rows, columns, and subjects.
#'
#' @param .src_list1 First source list to compare (considered the "base").
#' @param .src_list2 Second source list to compare (considered the "compare").
#' @param .subject_col Character string specifying the subject ID column name.
#'   Defaults to "USUBJID".
#'
#' @return A list with class "src_list_comparison" containing:
#' \describe{
#'   \item{domain_summary}{Tibble showing which domains are in each list}
#'   \item{domain_details}{Tibble with detailed comparison for common domains}
#'   \item{column_changes}{List of column additions/removals per domain}
#'   \item{subject_changes}{List of subject additions/removals per domain}
#' }
#'
#' @examples
#' \dontrun{
#' src_list1 <- read_src_dir(path1, .file_types = "xpt")
#' src_list2 <- read_src_dir(path2, .file_types = "xpt")
#' comparison <- compare_src_lists(src_list1, src_list2)
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

  # Categorize domains
  domains_only_in_1 <- setdiff(domains1, domains2)
  domains_only_in_2 <- setdiff(domains2, domains1)
  domains_common <- intersect(domains1, domains2)

  # Build domain summary
  all_domains <- sort(unique(c(domains1, domains2)))

  domain_summary <- dplyr::tibble(
    DOMAIN = all_domains,
    IN_LIST1 = all_domains %in% domains1,
    IN_LIST2 = all_domains %in% domains2,
    STATUS = dplyr::case_when(
      all_domains %in% domains_common ~ "common",
      all_domains %in% domains_only_in_1 ~ "only_in_list1",
      all_domains %in% domains_only_in_2 ~ "only_in_list2"
    )
  )

  # Detailed comparison for common domains
  domain_details <- dplyr::tibble(
    DOMAIN = character(),
    NROW_LIST1 = integer(),
    NROW_LIST2 = integer(),
    NROW_DIFF = integer(),
    NCOL_LIST1 = integer(),
    NCOL_LIST2 = integer(),
    NCOL_DIFF = integer(),
    N_SUBJECTS_LIST1 = integer(),
    N_SUBJECTS_LIST2 = integer(),
    N_SUBJECTS_DIFF = integer(),
    HAS_SUBJECT_COL = logical()
  )

  column_changes <- list()
  subject_changes <- list()

  if (length(domains_common) > 0) {
    cli::cli_progress_bar("Comparing domains", total = length(domains_common))

    for (domain.i in domains_common) {
      cli::cli_progress_update()

      df1 <- .src_list1[[domain.i]]
      df2 <- .src_list2[[domain.i]]

      # Skip non-data.frame elements
      if (!is.data.frame(df1) || !is.data.frame(df2)) {
        next
      }

      nrow1 <- nrow(df1)
      nrow2 <- nrow(df2)
      ncol1 <- ncol(df1)
      ncol2 <- ncol(df2)

      # Column comparison
      cols1 <- names(df1)
      cols2 <- names(df2)
      cols_added <- setdiff(cols2, cols1)
      cols_removed <- setdiff(cols1, cols2)

      if (length(cols_added) > 0 || length(cols_removed) > 0) {
        column_changes[[domain.i]] <- list(
          added = cols_added,
          removed = cols_removed
        )
      }

      # Subject comparison
      has_subject_col <- .subject_col %in% cols1 && .subject_col %in% cols2
      n_subjects1 <- NA_integer_
      n_subjects2 <- NA_integer_
      n_subjects_diff <- NA_integer_

      if (has_subject_col) {
        subjects1 <- unique(df1[[.subject_col]])
        subjects2 <- unique(df2[[.subject_col]])
        n_subjects1 <- length(subjects1)
        n_subjects2 <- length(subjects2)
        n_subjects_diff <- n_subjects2 - n_subjects1

        subjects_added <- setdiff(subjects2, subjects1)
        subjects_removed <- setdiff(subjects1, subjects2)

        if (length(subjects_added) > 0 || length(subjects_removed) > 0) {
          subject_changes[[domain.i]] <- list(
            added = subjects_added,
            removed = subjects_removed,
            n_added = length(subjects_added),
            n_removed = length(subjects_removed)
          )
        }
      }

      domain_details <- dplyr::bind_rows(
        domain_details,
        dplyr::tibble(
          DOMAIN = domain.i,
          NROW_LIST1 = nrow1,
          NROW_LIST2 = nrow2,
          NROW_DIFF = nrow2 - nrow1,
          NCOL_LIST1 = ncol1,
          NCOL_LIST2 = ncol2,
          NCOL_DIFF = ncol2 - ncol1,
          N_SUBJECTS_LIST1 = n_subjects1,
          N_SUBJECTS_LIST2 = n_subjects2,
          N_SUBJECTS_DIFF = n_subjects_diff,
          HAS_SUBJECT_COL = has_subject_col
        )
      )
    }

    cli::cli_progress_done()
  }

  # Build output
  out <- list(
    domain_summary = domain_summary,
    domain_details = domain_details,
    column_changes = column_changes,
    subject_changes = subject_changes,
    subject_col = .subject_col
  )

  class(out) <- c("src_list_comparison", "list")

  # Print summary
  print_src_list_comparison(out)

  invisible(out)
}


#' Print method for src_list_comparison
#'
#' @param x A src_list_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#' @export
print.src_list_comparison <- function(x, ...) {
  print_src_list_comparison(x)
  invisible(x)
}


#' Internal function to print comparison summary
#'
#' @param x A src_list_comparison object
#' @keywords internal
#' @noRd
print_src_list_comparison <- function(x) {
  cli::cli_h1("Source List Comparison")

  # Domain overview
  n_common <- sum(x$domain_summary$STATUS == "common")
  n_only1 <- sum(x$domain_summary$STATUS == "only_in_list1")
  n_only2 <- sum(x$domain_summary$STATUS == "only_in_list2")


  cli::cli_h2("Domain Overview")
  cli::cli_ul(c(
    "Common domains: {n_common}",
    "Domains only in list1: {n_only1}",
    "Domains only in list2: {n_only2}"
  ))

  if (n_only1 > 0) {
    domains_only1 <- x$domain_summary$DOMAIN[x$domain_summary$STATUS == "only_in_list1"]
    cli::cli_alert_warning("Domains only in list1: {paste(domains_only1, collapse = ', ')}")
  }

  if (n_only2 > 0) {
    domains_only2 <- x$domain_summary$DOMAIN[x$domain_summary$STATUS == "only_in_list2"]
    cli::cli_alert_warning("Domains only in list2: {paste(domains_only2, collapse = ', ')}")
  }

  # Row/Column/Subject changes
  if (nrow(x$domain_details) > 0) {
    cli::cli_h2("Domain Details (Common Domains)")

    # Summary of changes
    total_row_diff <- sum(x$domain_details$NROW_DIFF, na.rm = TRUE)
    domains_with_row_change <- sum(x$domain_details$NROW_DIFF != 0, na.rm = TRUE)
    domains_with_col_change <- sum(x$domain_details$NCOL_DIFF != 0, na.rm = TRUE)
    domains_with_subj_change <- sum(x$domain_details$N_SUBJECTS_DIFF != 0, na.rm = TRUE)

    cli::cli_ul(c(
      "Total row difference: {format_diff(total_row_diff)}",
      "Domains with row changes: {domains_with_row_change}",
      "Domains with column changes: {domains_with_col_change}",
      "Domains with subject changes: {domains_with_subj_change}"
    ))

    # Show domains with significant changes
    changed_domains <- x$domain_details %>%
      dplyr::filter(
        .data$NROW_DIFF != 0 |
        .data$NCOL_DIFF != 0 |
        (!is.na(.data$N_SUBJECTS_DIFF) & .data$N_SUBJECTS_DIFF != 0)
      )

    if (nrow(changed_domains) > 0) {
      cli::cli_h3("Domains with Changes")
      for (i in seq_len(nrow(changed_domains))) {
        row.i <- changed_domains[i, ]
        cli::cli_alert_info("{row.i$DOMAIN}: rows {format_diff(row.i$NROW_DIFF)}, cols {format_diff(row.i$NCOL_DIFF)}, subjects {format_diff(row.i$N_SUBJECTS_DIFF)}")
      }
    }
  }

  # Column changes detail
  if (length(x$column_changes) > 0) {
    cli::cli_h2("Column Changes")
    for (domain.i in names(x$column_changes)) {
      changes.i <- x$column_changes[[domain.i]]
      if (length(changes.i$added) > 0) {
        cli::cli_alert_success("{domain.i}: Added columns: {paste(changes.i$added, collapse = ', ')}")
      }
      if (length(changes.i$removed) > 0) {
        cli::cli_alert_danger("{domain.i}: Removed columns: {paste(changes.i$removed, collapse = ', ')}")
      }
    }
  }

  # Subject changes summary
  if (length(x$subject_changes) > 0) {
    cli::cli_h2("Subject Changes (using {x$subject_col})")
    for (domain.i in names(x$subject_changes)) {
      changes.i <- x$subject_changes[[domain.i]]
      if (changes.i$n_added > 0) {
        cli::cli_alert_success("{domain.i}: {changes.i$n_added} subject(s) added")
      }
      if (changes.i$n_removed > 0) {
        cli::cli_alert_danger("{domain.i}: {changes.i$n_removed} subject(s) removed")
      }
    }
  }
}


#' Format a numeric difference for display
#'
#' @param x Numeric value
#' @return Character string with + or - prefix
#' @keywords internal
#' @noRd
format_diff <- function(x) {
  if (is.na(x)) return("NA")
  if (x > 0) return(paste0("+", x))
  if (x < 0) return(as.character(x))
  return("0")
}
