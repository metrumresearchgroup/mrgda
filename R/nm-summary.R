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
    obj1 <- .data %>%
      dplyr::select(c(STUDY = flags$study, flags$bl_cov_cont)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = flags$bl_cov_cont) %>%
      dplyr::group_by(STUDY, name) %>%
      dplyr::mutate(
        MEAN = signif(mean(value), 3),
        MAX = signif(max(value), 3),
        MIN = signif(min(value), 3)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(shorts) %>%
      dplyr::distinct(STUDY, BLCOV = short, MIN, MEAN, MAX) %>%
      dplyr::arrange(BLCOV)
  }

  # largest baseline continuous covariates
  if (length(flags$bl_cov_cont) > 0) {
    obj2 <- .data %>%
      dplyr::select(c(ID = flags$id, STUDY = flags$study, flags$bl_cov_cont)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = flags$bl_cov_cont) %>%
      dplyr::group_by(name) %>%
      dplyr::arrange(-value) %>%
      dplyr::slice(1:5) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(shorts) %>%
      dplyr::mutate(value = signif(value, 3)) %>%
      dplyr::distinct(ID, STUDY, BLCOV = short, Value = value) %>%
      dplyr::arrange(BLCOV, -Value)
  }

  # smallest baseline continuous covariates
  if (length(flags$bl_cov_cont) > 0) {
    obj3 <- .data %>%
      dplyr::select(c(ID = flags$id, STUDY = flags$study, flags$bl_cov_cont)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(cols = flags$bl_cov_cont) %>%
      dplyr::group_by(name) %>%
      dplyr::arrange(value) %>%
      dplyr::slice(1:5) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(shorts) %>%
      dplyr::mutate(value = signif(value, 3)) %>%
      dplyr::distinct(ID, STUDY, BLCOV = short, Value = value) %>%
      dplyr::arrange(BLCOV, Value)
  }

  # Output pdf --------------------------------------------------------------

  returnlist[["1"]] <- pmtables::stable_long(obj1, panel = pmtables::as.panel("BLCOV"),
                                        lt_cap_text = "Summary of baseline continuous covariates by study")

  returnlist[["2"]] <- pmtables::stable_long(obj2, panel = pmtables::as.panel("BLCOV"),
                                             lt_cap_text = "Summary of highest baseline continuous covariate values")

  returnlist[["3"]] <- pmtables::stable_long(obj3, panel = pmtables::as.panel("BLCOV"),
                                             lt_cap_text = "Summary of smallest baseline continuous covariate values")

  pmtables::st2article(returnlist, ntex=2)

}
