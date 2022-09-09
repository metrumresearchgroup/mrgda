temppdf <- function(..., .list = NULL, ntex = 1,  #nocov start
                       stem = "view-st2article",
                       output_dir = tempdir(), template = NULL,
                       margin = c("2.54cm", "3cm"), caption = NULL,
                       dry_run = FALSE, stdout = FALSE, show_pdf = TRUE) {

  tables <- c(list(...),.list)
  tables <- flatten_if(tables, is.list)
  names(tables) <- tab_escape(names(tables))
  inputs <- tables
  output_dir <- normalizePath(output_dir)
  build_dir <- normalizePath(tempdir())
  assert_that(dir.exists(output_dir))
  assert_that(is.character(margin))
  if(length(margin)==1) {
    margin <- c(margin, margin)
  }
  assert_that(length(margin)==2)
  tables_are_stable <- map_lgl(tables, inherits, what = "stable")
  assert_that(all(tables_are_stable))

  if(is.null(template)) {
    template <- system.file("tex", "article.tex", package = "pmtables")
  } else {
    assert_that(file.exists(template))
  }

  texfile <- paste0(stem,".tex")
  pdffile <- paste0(stem,".pdf")
  st2article_input <- paste0(stem,"-tables.tex")

  temp <- readLines(template)

  env <- list()
  env$list_of_tables <- c("\\listoftables", "\\clearpage")
  env$input_file <- st2article_input
  env$hmargin <- margin[1]
  env$vmargin <- margin[2]

  if(length(tables)==1 || is.null(caption)) {
    env$list_of_tables <- "% listoftables"
  }

  temp <- mgluet(temp, .envir = env)

  if(is.character(caption)) {
    wrap_with_caption <- function(text, i) {
      if(is.null(i)) i <- "<no name given>"
      short <- paste0("pmtables output preview - ", i)
      pt_wrap(
        text, context = "tex", caption = caption, short = short,
        con = NULL
      )
    }
    tables <- imap(tables, wrap_with_caption)
  } else {
    tables <- map(tables, pt_wrap, context = "tex", con = NULL)
  }

  tables <- map(tables, ~ c(.x, "\\clearpage"))
  tables <- flatten_chr(tables)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(build_dir)

  writeLines(tables,st2article_input)
  writeLines(temp,texfile)
  ans <- list(doc = temp, tables = tables)

  if(dry_run) return(inputs)

  if(file.exists(pdffile)) {
    unlink(pdffile)
  }

  if(file.exists(texfile)) {
    for(i in seq_len(ntex)) {
      result <- system2(
        "pdflatex",
        args=c("-halt-on-error ",texfile), stdout = stdout
      )
      if(!identical(result, 0L)) {
        warning("non-zero exit from pdflatex", call.=FALSE)
      }
    }
  } else {
    stop(
      'could not locate the template tex file; ',
      'pass `stdout=""` to see pdflatex build output',
      call.=FALSE
    )
  }

  pdf_output <- file.path(output_dir,pdffile)

  if(output_dir != build_dir) {
    file.copy(pdffile, pdf_output, overwrite = TRUE)
  }

  if(isTRUE(show_pdf)) {
    if(file.exists(pdf_output)) {
      fs::file_show(pdf_output)
    } else {
      stop("could not locate the output pdf file", call. = FALSE)
    }
  }

  return(invisible(inputs))
} # nocov end

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


  # tables ------------------------------------------------------------------

  obj1 <- .data %>%
    dplyr::select(c(flags$study, flags$bl_cov_cont)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(flags$study)) %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(cols = flags$bl_cov_cont)

  # Output pdf --------------------------------------------------------------

  returnlist[["1"]] <- pmtables::stable(obj1)

  pmtables::st2article(returnlist)

}
