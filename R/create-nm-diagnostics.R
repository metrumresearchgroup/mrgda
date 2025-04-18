#' Subject-level NONMEM diagnostic PDF
#'
#' Creates a multi-page PDF of subject-level diagnostics for NONMEM data.
#'
#' **Per-subject layout**
#' 1. Metadata table (`.subject_cols`) decoded with `yspec`.
#' 2. Concentration vs. time with vertical dose lines (EVID = 0 observations only).
#' 3. Concentration vs. time-after-dose coloured by occasion (EVID = 0 observations only).
#'
#' An `OCC` column is added automatically via `lastdose::lastdose()` when absent.
#'
#' @param .data         Data frame of dosing and observation records.
#' @param .subject_cols Character vector of columns to show in the table.
#' @param .spec         `yspec` object for categorical decoding.
#' @param .subject_col  Subject identifier column (default "ID").
#' @param .outdir       Existing directory for the PDF.
#' @param .filename     Output PDF file name (default "nm-diagnostics.pdf").
#'
#' @return (Invisible) full path to the PDF.
#' @importFrom utils flush.console
#' @export
create_nm_diagnostics <- function(
    .data,
    .subject_cols,
    .spec,
    .subject_col = "ID",
    .outdir      = ".",
    .filename    = "nm-diagnostics.pdf"
) {

  if (!base::dir.exists(.outdir)) {
    stop(glue::glue("Output directory '{.outdir}' does not exist."), call. = FALSE)
  }

  if (!"OCC" %in% base::names(.data)) {
    .data <- lastdose::lastdose(.data, include_occ = TRUE)
  }

  pdf_file <- base::file.path(.outdir, .filename)
  grDevices::pdf(pdf_file, width = 10, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)

  ids   <- base::unique(.data[[.subject_col]])
  n_sub <- base::length(ids)
  pb <- progress::progress_bar$new(
    format   = "ID :id [:bar] :percent (:current/:total), ETA: :eta",
    total    = n_sub,
    clear    = FALSE,
    width    = 60,
    stream   = base::stdout()
  )

  for (id.i in ids) {
    df.i <- dplyr::filter(.data, .data[[.subject_col]] == id.i)

    sl_tbl.i <- df.i %>%
      dplyr::select(dplyr::all_of(.subject_cols)) %>%
      dplyr::distinct() %>%
      dplyr::slice_head(n = 1) %>%
      yspec::ys_factors(spec = .spec, .keep_values = FALSE) %>%
      gridExtra::tableGrob(
        rows  = NULL,
        theme = gridExtra::ttheme_default(base_size = 8)
      )

    vlines <- df.i$TIME[df.i$EVID == 1]

    p_tbl.i <- ggplot2::ggplot() +
      ggplot2::annotation_custom(sl_tbl.i) +
      ggplot2::labs(title = glue::glue("ID {id.i} - Subject-level Table")) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 1, 20), "pt"))

    p_ct.i <- df.i %>%
      dplyr::filter(EVID == 0) %>%
      ggplot2::ggplot(ggplot2::aes(TIME, DV)) +
      ggplot2::geom_vline(xintercept = vlines, linetype = 2) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        title   = glue::glue("ID {id.i} - Conc vs Time"),
        caption = "Only EVID = 0 records shown; dashed lines are doses"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(1, 20, 0, 20), "pt"))

    p_tad.i <- df.i %>%
      dplyr::filter(EVID == 0) %>%
      ggplot2::ggplot(ggplot2::aes(TAD, DV, color = factor(OCC))) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        title   = glue::glue("ID {id.i} - Conc vs TAD"),
        color   = "Occasion",
        caption = "Only EVID = 0 records shown"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 5, 20), "pt"))

    patchwork::wrap_plots(
      p_tbl.i,
      p_ct.i,
      p_tad.i,
      ncol    = 1,
      heights = c(0.4, 1, 1.3)
    ) %>% print()

    pb$tick(tokens = list(id = id.i))
    utils::flush.console()
  }

  base::message("Written PDF to: ", pdf_file)
  invisible(pdf_file)
}
