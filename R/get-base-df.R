#' Get Base Data Frame
#'
#' This function is used to get the base dataframe from a previous file or from an svn file.
#' If the `.compare_from_svn` flag is set to TRUE, it will try to export the previous file from svn.
#' If the file does not exist, it will return NULL.
#'
#' @param .prev_file A string indicating the path of the previous file.
#' @param .compare_from_svn A boolean flag to determine if the function should compare from svn.
#'
#' @return A list containing two elements: 'base_df' and 'from_svn'.
#' 'base_df' is the dataframe read from the file (or NULL if the file does not exist).
#' 'from_svn' is a boolean indicating whether the file was successfully exported from svn.
#'
#' @keywords internal
get_base_df <- function(.prev_file, .compare_from_svn){

  base <- .prev_file
  from_svn <- FALSE
  prev_rev <- NA_character_

  if (.compare_from_svn) {

    cur_dir <- getwd()
    on.exit(setwd(cur_dir))
    setwd(rprojroot::find_rstudio_root_file())

    base_temp <- tempfile(fileext = ".csv")

    svn_info <- tryCatch(
      system2("svn", c("info", "-r", "HEAD", base), stdout = TRUE, stderr = TRUE),
      error = function(e) e
    )

    if (inherits(svn_info, "error")) {
      warning("SVN unavailable; unable to retrieve revision info for ", base, call. = FALSE)
    } else {
      rev_line <- svn_info[grepl("^Revision:", svn_info)]
      if (length(rev_line) == 0) {
        warning("SVN info did not include a revision for ", base, call. = FALSE)
      } else {
        prev_rev <- sub("^Revision:\\s*", "", rev_line[[1]])
      }
    }

    if (!is.na(prev_rev)) {
      export_try <- tryCatch(
        system2("svn", c("export", "-r", prev_rev, base, base_temp), stdout = TRUE, stderr = TRUE),
        error = function(e) e
      )
      export_status <- attr(export_try, "status")
      if (!inherits(export_try, "error") && (is.null(export_status) || export_status == 0)) {
        from_svn <- TRUE
        base <- base_temp
      } else {
        warning("SVN export failed for ", base, call. = FALSE)
      }
    }

  }

  base_df <-
    if (file.exists(base)) {
      read_csv_dots(base)
    } else {
      NULL
    }


  prev_rev <- ifelse(length(prev_rev) == 0, NA, prev_rev)

  return(
    list(
      base_df = base_df,
      from_svn = from_svn,
      prev_rev = prev_rev
    )
  )

}
