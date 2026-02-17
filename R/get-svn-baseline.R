#' Get Base Data Frame
#'
#' This function is used to get the base dataframe from a previous file or from an svn file.
#' If the `.compare_from_svn` flag is set to TRUE, it will try to export the previous file from svn.
#' If the file does not exist, it will return NULL.
#'
#' @param .prev_file A string indicating the path of the previous file.
#' @param .compare_from_svn A boolean flag to determine if the function should compare from svn.
#' @param .reader A function used to read the file. Defaults to `read_csv_dots`.
#' @param .file_ext A string indicating the file extension for the temp file. Defaults to `".csv"`.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{base_df}: the dataframe read from the file (or NULL if the file does not exist).
#'   \item \code{from_svn}: a boolean indicating whether the file was successfully exported from svn.
#'   \item \code{prev_rev}: the SVN revision number (or NA).
#'   \item \code{svn_author}: the last-changed author from SVN (or NA).
#'   \item \code{svn_date}: the last-changed date from SVN (or NA).
#' }
#'
#' @keywords internal
get_svn_baseline <- function(.prev_file, .compare_from_svn, .reader = read_csv_dots, .file_ext = ".csv"){

  base <- .prev_file
  from_svn <- FALSE
  prev_rev <- NA
  svn_author <- NA_character_
  svn_date <- NA_character_

  if (.compare_from_svn) {

    cur_dir <- getwd()
    on.exit(setwd(cur_dir))
    setwd(rprojroot::find_rstudio_root_file())

    base_temp <- tempfile(fileext = .file_ext)

    svn_info_raw <- try(
      system(paste0("svn info 2>/dev/null -r HEAD ", base), intern = TRUE),
      silent = TRUE
    )

    if (length(svn_info_raw) > 0 && !inherits(svn_info_raw, "try-error")) {
      rev_line <- grep("^Revision: ", svn_info_raw, value = TRUE)
      if (length(rev_line) > 0) prev_rev <- sub("^Revision: ", "", rev_line)

      author_line <- grep("^Last Changed Author: ", svn_info_raw, value = TRUE)
      if (length(author_line) > 0) svn_author <- sub("^Last Changed Author: ", "", author_line)

      date_line <- grep("^Last Changed Date: ", svn_info_raw, value = TRUE)
      if (length(date_line) > 0) {
        svn_date <- sub(" \\(.*\\)$", "", sub("^Last Changed Date: ", "", date_line))
      }
    }

    if (length(prev_rev) > 0 && !is.na(prev_rev)) {

      export_try <- try(system(paste0("svn export -r ", prev_rev, " ", base ," ", base_temp, " > /dev/null 2>&1")))

      if (export_try == 0) {
        from_svn <- TRUE
        base <- base_temp
      }
    }

  }

  base_df <-
    if (file.exists(base)) {
      .reader(base)
    } else {
      NULL
    }


  prev_rev <- ifelse(length(prev_rev) == 0, NA, prev_rev)

  return(
    list(
      base_df = base_df,
      from_svn = from_svn,
      prev_rev = prev_rev,
      svn_author = svn_author,
      svn_date = svn_date
    )
  )

}
