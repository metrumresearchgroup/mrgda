#' Provide diagnostic summary of an NMTRAN dataset
#'
#' @description
#' This function is intended to provide a high level overview of variables
#' derived during a data assembly. The output of this function will provide
#' the user with a pdf file containing a series of tables to help the user
#' better understand distributions within the derived data set.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param ... arguments passed through from methods (currently none)
#' @param .type specify desired output of "tables" or "figures". Default is "tables"
#' @param .figure_prompt whether graphics device asks for confirmation between figures
#' @examples
#'
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#'
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#'
#' # To change the output to summary figures instead of tables
#' nm_summary(.data = nm, .spec = nm_spec, .type = "figures", .figure_prompt = FALSE)
#'
#' @md
#' @export
nm_summary <- function(.data,
                       .spec,
                       ...,
                       .type = "tables",
                       .figure_prompt = TRUE){

  .data <- .data %>% yspec::ys_add_factors(.spec, .suffix = "")

  g_r <- gather_flags(.data, .spec)

  shorts <-
    dplyr::bind_rows(yspec::ys_get_short_unit(.spec)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(.data = .) %>%
    dplyr::rename(name = rowname, short = V1)

  subject_level_data <-
    .data %>%
    dplyr::group_by(dplyr::across(g_r$flags$id)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # tables ------------------------------------------------------------------
  returnlist <- list()

  # baseline continuous covariates
  returnlist[["1"]] <-
    subject_level_data %>%
    dplyr::select(c(g_r$flags$id, g_r$flags$study, g_r$flags$bl_cov_cont)) %>%
    tidyr::pivot_longer(cols = g_r$flags$bl_cov_cont) %>%
    dplyr::group_by(dplyr::across(c(g_r$flags$study, "name"))) %>%
    dplyr::summarise(
      MEAN = signif(mean(value), 3),
      MAX = signif(max(value), 3),
      MIN = signif(min(value), 3)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(shorts) %>%
    dplyr::mutate(
      short = dplyr::if_else(is.na(short), "Missing flag", short)
    ) %>%
    dplyr::select(
      c(g_r$flags$study, "short", "MIN", "MEAN", "MAX")
    ) %>%
    dplyr::arrange(short) %>%
    dplyr::mutate(
      PANEL = "short",
      LT_CAP_TEXT = "Summary of baseline continuous covariates by study"
    )

  # baseline categorical covariates
  returnlist[["2"]] <-
    subject_level_data %>%
    dplyr::select(
      c(g_r$flags$id,
        g_r$flags$study,
        g_r$flags$bl_cov_cat)
    ) %>%
    tidyr::pivot_longer(cols = g_r$flags$bl_cov_cat) %>%
    dplyr::group_by(dplyr::across(c(g_r$flags$study, "name"))) %>%
    dplyr::count(value) %>%
    dplyr::mutate(n = round(n/sum(n)*100, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(shorts) %>%
    dplyr::mutate(
      short = dplyr::if_else(is.na(short), "Missing flag", short)
    ) %>%
    dplyr::distinct(
      dplyr::across(c(g_r$flags$study, "short", "value", "n"))
    ) %>%
    dplyr::arrange(-n) %>%
    dplyr::rename(Percent = n) %>%
    dplyr::arrange(dplyr::across(c("short", g_r$flags$study))) %>%
    tidyr::unite("BLCAT", c(g_r$flags$study, "short"), sep = ": ") %>%
    dplyr::mutate(
      PANEL = "BLCAT",
      LT_CAP_TEXT = "Summary of baseline categorical covariates by study"
    )

  # primary keys
  returnlist[["3"]] <-
    g_r$data %>%
    dplyr::count(dplyr::across(c(g_r$flags$primary_keys))) %>%
    dplyr::mutate(Placeholder = "Full data") %>%
    dplyr::mutate(
      PANEL = "Placeholder",
      LT_CAP_TEXT = "Summary of primary keys"
    )

  # figures -----------------------------------------------------------------
  figurelist <- list()

  # baseline continuous covariates
  plot_num <- 1

  covnums <-
    g_r$data %>%
    dplyr::select(c(g_r$flags$bl_cov_cont)) %>%
    tidyr::pivot_longer(
      cols = g_r$flags$bl_cov_cont,
      names_to = "BLCOV",
      values_to = "BLCOV_VAL"
    ) %>%
    dplyr::distinct(BLCOV) %>%
    dplyr::mutate(NUM = 1:dplyr::n())

  blcont_covs <-
    subject_level_data %>%
    dplyr::select(
      c(g_r$flags$id,
        STUDY = g_r$flags$study,
        g_r$flags$bl_cov_cont)
    ) %>%
    tidyr::pivot_longer(
      cols = g_r$flags$bl_cov_cont,
      names_to = "BLCOV",
      values_to = "BLCOV_VAL"
    ) %>%
    dplyr::left_join(covnums) %>%
    dplyr::mutate(GROUPING = ceiling(NUM/9))

  for (i in unique(blcont_covs$GROUPING)) {
    figurelist[[glue::glue({plot_num})]] <-
      blcont_covs %>%
      dplyr::filter(GROUPING == i) %>%
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = STUDY, y = BLCOV_VAL)) +
      ggplot2::facet_wrap(~BLCOV, nrow = 3, ncol = 3, scales = "free") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)
      )
    plot_num <- plot_num + 1
  }

  # Categorical figures
  catnums <-
    g_r$data %>%
    dplyr::select(c(g_r$flags$bl_cov_cat)) %>%
    tidyr::pivot_longer(
      cols = g_r$flags$bl_cov_cat,
      names_to = "BLCAT",
      values_to = "BLCAT_VAL"
    ) %>%
    dplyr::distinct(BLCAT) %>%
    dplyr::mutate(NUM = 1:dplyr::n())

  blcat_covs <-
    subject_level_data %>%
    dplyr::select(
      c(g_r$flags$id,
        STUDY = g_r$flags$study,
        g_r$flags$bl_cov_cat)
    ) %>%
    tidyr::pivot_longer(
      cols = g_r$flags$bl_cov_cat,
      names_to = "BLCAT",
      values_to = "BLCAT_VAL"
    ) %>%
    dplyr::left_join(catnums) %>%
    dplyr::mutate(GROUPING = ceiling(NUM/6))

  for (i in unique(blcat_covs$GROUPING)) {
    figurelist[[glue::glue({plot_num})]] <-
      blcat_covs %>%
      dplyr::filter(GROUPING == i) %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(x = BLCAT_VAL, fill = STUDY),
        position = "dodge"
      ) +
      ggplot2::facet_wrap(~BLCAT, nrow = 3, ncol = 3, scales = "free") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
      )
    plot_num <- plot_num + 1
  }

  # output ------------------------------------------------------------------
  if (.type == "tables") {
    class(returnlist) <- c("nm_summary_results", class(returnlist))

    return(returnlist)
  }

  if (.type == "figures") {

    for (plot in names(figurelist)) {

      print(figurelist[[plot]])

      if (.figure_prompt) {
        readline(prompt = "Press [enter] to see next figure")
      }

    }

    return(figurelist)

  }

}

#' @method print nm_summary_results
#' @export
print.nm_summary_results <- function(x, ...) {
  returnlistStable <-
    purrr::map(
      x,
      ~ {

        if(all(is.na(.x[[unique(.x$PANEL)]]))) {
          .x[[unique(.x$PANEL)]] <- "Missing flags"
        }

        pmtables::stable_long(
          data = .x %>% dplyr::select(-PANEL, -LT_CAP_TEXT),
          panel = pmtables::as.panel(unique(.x$PANEL)),
          lt_cap_text = unique(.x$LT_CAP_TEXT)
        )

      }
    )

  pmtables::st2report(returnlistStable, ntex = length(x))
}
