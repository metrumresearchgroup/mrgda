#' @keywords internal
make_outlier_df <- function(.df, .group = FALSE, .id = "USUBJID", .time = NULL, .cov = "ALL", .show_mean_med = TRUE ){


  # group
  if (.group == FALSE){
    .df1 <- .df %>%
      dplyr::select(all_of(.id), .time, .cov)
  } else {
    .df1 <- .df %>%
      dplyr::select(all_of(.id), .group, .time, .cov) %>%
      dplyr::group_by(!!! dplyr::syms(.group))
  }

  #show mean and median
  if (length(.cov) > 1 & .show_mean_med == TRUE){
    .df2 <- .df1 %>%
      dplyr::mutate(MEAN = mean(!!dplyr::sym(.cov)),
             MEDIAN = median(!!dplyr::sym(.cov))
      ) %>%
      rstatix::identify_outliers(.cov) %>%
      dplyr::ungroup()

    .df_all <- list()

    for (i in 1:length(.cov)){
      .df.i <- .df1 %>%
        dplyr::mutate("MEAN_{i}" = mean(!!dplyr::sym(.cov[[i]]), na.rm=T),
                      MEDIAN = median(!!dplyr::sym(.cov[[i]]), na.rm=T)
        ) #%>%
        # dplyr::rename(glue::glue("{.cov[[1]]}") = MEAN)
      .df_all <- dplyr::bind_cols(.df.i)
    }
    #testing it out
    library(tidyverse)
    .df1a <- x

    .df1 %>%
      dplyr::select("LBORNRLO", "LBORNRHI") %>%
      lapply(., function(x){c(mean(x, na.rm=T),median(x, na.rm = T))})


      # mysummary <- function(x){
      #   funs <- c(mean(x, na.rm=T),median(x, na.rm = T))
      #   lapply(funs,function(funs) f(x,na.rm = T))
      # }

      mysummary <- function(x){c(mean(x, na.rm=T),median(x, na.rm = T))}

      lapply(df,mysummary2)
    colnames(.df1a) <- paste0('MEAN_', colnames(.df1a))
      dplyr::rename(~paste0(., "Text"))
      #dplyr::mutate(paste0("MEAN_", .cov) = colMeans(na.rm = TRUE))

    .df1 %>%
      dplyr::bind_cols(
        purrr::map_dfc(.x = .cov,
                .f = ~ .y %>%
                  #dplyr::rowwise() %>%
                  dplyr::transmute(!!stringr::str_c("MEAN_", .x) := mean((.x))),
                .y = .)
      )


    .df1 %>%
      purrr:::map(~ .cov %>%
            select(matches(.x)) %>%
            reduce(`+`)) %>%
      set_names(paste0("sum_", .df1)) %>%
      dplyr::bind_cols(.cov, .)


    dplyr::bind_rows(
      purrr::map(.cov,
                 ~ dplyr::mutate(data = df1,  mean(.x))
      ) %>%
        setNames(.cov))
  } else if (length(.cov) == 1 & .show_mean_med == TRUE){
    .df2 <- .df1 %>%
      dplyr::mutate(MEAN = mean(!!dplyr::sym(.cov)),
                    MEDIAN = median(!!dplyr::sym(.cov))
      ) %>%
      rstatix::identify_outliers(.cov) %>%
      dplyr::ungroup()
  } else {
    .df2 <- .df1 %>%
      rstatix::identify_outliers(.cov) %>%
      dplyr::ungroup()
  }

  .df2
}
