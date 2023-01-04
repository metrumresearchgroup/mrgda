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
#' @examples
#'
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#'
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#'
#' nm_summary(.data = nm, .spec = nm_spec)
#'
#' @md
#' @export
nm_summary <- function(.data, .spec){

  outputs <- list()

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
    dplyr::select(c(g_r$flags$id, g_r$flags$study, g_r$flags$bl_cat_cov, g_r$flags$bl_cont_cov)) %>%
    dplyr::distinct()

  # tables ------------------------------------------------------------------
  outputs$Tables <- list()

  # baseline continuous Covariates
  outputs$Tables$Covariates[["Baseline continuous Covariates"]] <-
    subject_level_data %>%
    dplyr::select(c(g_r$flags$id, g_r$flags$study, g_r$flags$bl_cont_cov)) %>%
    tidyr::pivot_longer(cols = g_r$flags$bl_cont_cov) %>%
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
    dplyr::group_by(short) %>%
    gt::gt() %>%
    suppressMessages()

  # baseline categorical Covariates
  outputs$Tables$Covariates[["Baseline categorical Covariates"]] <-
    subject_level_data %>%
    dplyr::select(
      c(g_r$flags$id,
        g_r$flags$study,
        g_r$flags$bl_cat_cov)
    ) %>%
    tidyr::pivot_longer(cols = g_r$flags$bl_cat_cov) %>%
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
    dplyr::arrange(dplyr::across(c(g_r$flags$study, "short", -"Percent"))) %>%
    dplyr::group_by(short, value) %>%
    gt::gt() %>%
    suppressMessages()

  # primary keys
  outputs$Tables$Miscellaneous[["Primary key summary"]] <-
    g_r$data %>%
    dplyr::count(dplyr::across(c(g_r$flags$primary_keys))) %>%
    dplyr::mutate(Placeholder = "Full data") %>%
    gt::gt()

  # figures -----------------------------------------------------------------
  outputs$Figures <- list()

  blcont_covs <-
    subject_level_data %>%
    dplyr::select(
      c(g_r$flags$id,
        STUDY = g_r$flags$study,
        g_r$flags$bl_cont_cov)
    ) %>%
    tidyr::pivot_longer(
      cols = g_r$flags$bl_cont_cov,
      names_to = "BLCOV",
      values_to = "BLCOV_VAL"
    ) %>%
    dplyr::left_join(shorts %>% dplyr::rename(BLCOV = name)) %>%
    suppressMessages()

  for (i in unique(blcont_covs$short)) {
    outputs$Figures$Boxplots[[i]] <-
      blcont_covs %>%
      dplyr::filter(short == i) %>%
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = STUDY, y = BLCOV_VAL)) +
      #ggplot2::geom_jitter(ggplot2::aes(x = STUDY, y = BLCOV_VAL), height = 0, width = 0.1) +
      ggplot2::facet_wrap(~short)
  }


  # output ------------------------------------------------------------------
  nm_summary_temp <- tempfile(fileext = ".html")

  rmarkdown::render(
    input = system.file("templates/nm-summary.Rmd", package = "mrgda"),
    output_file = nm_summary_temp,
    params = list(outputs = outputs),
    envir = new.env(),
    quiet = TRUE
  )

  if(interactive()){
    browseURL(nm_summary_temp)
  }

  return(outputs)
}

