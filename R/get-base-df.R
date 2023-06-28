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

  if (.compare_from_svn) {

    cur_dir <- getwd()
    on.exit(setwd(cur_dir))
    setwd(here::here())

    base_temp <- tempfile(fileext = ".csv")

    prev_rev <- try(
      system(paste0("svn info -r HEAD ", base, " | grep Revision | awk '{print $2}'"), intern = TRUE)
    )

    if (length(prev_rev) > 0) {

      export_try <- try(system(paste0("svn export -r ", prev_rev, " ", base ," ", base_temp, " > /dev/null 2>&1")))

      if (export_try == 0) {
        from_svn <- TRUE
        base <- base_temp
      }
    }

  }

  base_df <-
    if (file.exists(base)) {
      readr::read_csv(base) %>% suppressMessages()
    } else {
      NULL
    }

  return(
    list(
      base_df = base_df,
      from_svn = from_svn
    )
  )

}
