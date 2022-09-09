#' Provide diagnostic summary of an NMTRAN dataset
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @examples
#'
#'
#' @md
#' @export
nm_summary <- function(.data, .spec){

  returnlist <- list()

  .data <- .data %>% yspec::ys_add_factors(.spec, .suffix = "")

  # gather flags ------------------------------------------------------------
  recognized_flags <-
    c(
      "id",
      "study",
      "primary_keys",
      "time",
      "bl_cov_cat",
      "bl_cov_cont",
      "tv_cov_cat",
      "tv_cov_cont"
    )

  flags <-
    yspec::pull_meta(.spec, "flags")[recognized_flags] %>%
    purrr::set_names(recognized_flags)

  shorts <- bind_rows(yspec::ys_get_short_unit(.spec)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>% rename(name = rowname, short = V1)


  # tables ------------------------------------------------------------------

  # baseline continuous covariates
  if (length(flags$bl_cov_cont) > 0) {
    returnlist[["1"]] <- .data %>%
      dplyr::select(c(flags$study, flags$bl_cov_cont)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = flags$bl_cov_cont) %>%
      dplyr::group_by(across(c(flags$study, "name"))) %>%
      dplyr::summarise(
        MEAN = signif(mean(value), 3),
        MAX = signif(max(value), 3),
        MIN = signif(min(value), 3)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(shorts) %>%
      dplyr::select(c(flags$study, "short", "MIN", "MEAN", "MAX")) %>%
      dplyr::arrange(short) %>%
      dplyr::mutate(
        PANEL = "short",
        LT_CAP_TEXT = "Summary of baseline continuous covariates by study"
      )
  }

  # baseline categorical covariates
  if (length(flags$bl_cov_cat) > 0) {
    returnlist[["2"]] <- .data %>%
      dplyr::select(c(flags$id, flags$study, flags$bl_cov_cat)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = flags$bl_cov_cat) %>%
      dplyr::group_by(across(c(flags$study, "name"))) %>%
      dplyr::count(value) %>%
      dplyr::mutate(n = round(n/sum(n)*100, 2)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(shorts) %>%
      dplyr::distinct(across(c(flags$study, "short", "value", "n"))) %>%
      dplyr::arrange(-n) %>%
      dplyr::arrange(across(c("short", flags$study))) %>%
      tidyr::unite("BLCAT", c(flags$study, "short"), sep = ": ") %>%
      dplyr::mutate(
        PANEL = "BLCAT",
        LT_CAP_TEXT = "Summary of baseline categorical covariates by study"
      )
  }

  # baseline categorical covariates
  if (length(flags$primary_keys) > 0) {
    returnlist[["3"]] <- .data %>%
      dplyr::count(across(c(flags$primary_keys))) %>%
      dplyr::mutate(Placeholder = "Full data") %>%
      dplyr::mutate(
        PANEL = "Placeholder",
        LT_CAP_TEXT = "Summary of primary keys"
      )
  }

  # Output ------------------------------------------------------------------
  class(returnlist) <- c("nm_validate_summary_results", class(returnlist))

  return(returnlist)

}

#' @method print nm_validate_results
#' @export
print.nm_validate_summary_results <- function(x, ...) {
  returnlistStable <-
    purrr::map(
      x,
      ~ pmtables::stable_long(
        data = .x %>% select(-PANEL, -LT_CAP_TEXT),
        panel = pmtables::as.panel(unique(.x$PANEL)),
        lt_cap_text = unique(.x$LT_CAP_TEXT)
      )
    )

  pmtables::st2report(returnlistStable, ntex=length(x))
}
