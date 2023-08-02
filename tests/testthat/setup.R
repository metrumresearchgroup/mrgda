
#' Creates a temporary directory for running svn commands
#'
#' Avoids `renv` issues associated with `tempdir()`
#'
#' @param clean Logical (`TRUE`/`FALSE`). Indicates if the temporary directory should be deleted after use
#' @param env Environment to use for scoping
#'
#' @keywords internal
local_svn_repo <- function(clean = TRUE, env = parent.frame()){
  repo <- withr::local_tempdir("svn-test-repo",
                               clean = clean,
                               .local_envir = env
  )
  return(repo)
}


#' Helper function for executing code in a temporary directory
#'
#' Useful when you dont need to refer to the directory by name
#'
#' @param code Code to execute in the temporary environment
#' @inheritParams local_svn_repo
#'
#' @keywords internal
svn_repo_with_code <- function(code, clean = TRUE, env = parent.frame()) {
  repo <- local_svn_repo(clean = clean, env = env)
  if (isTRUE(clean)) {
    withr::defer(unlink(repo, recursive = TRUE))
  }

  withr::with_dir(repo, code)
}

#' Compare inferred data class from v(), to what shows in a tibble view
#'
#' @param df a starting dataframe
#' @param df_view a DT::datatable object returned from v()
#'
#' @keywords internal
map_v_classes <- function(df, df_view){
  # Regex to extract column names and classes from datatable container
  pattern <- "<th>(.*?)<br>.*?&lt;(.*?)&gt;.*?</th>"

  column_info_df <- purrr::map_df(
    stringr::str_match_all(df_view$x$container, pattern), ~
      data.frame(col_name = .x[, 2], determined_class = .x[, 3])
  ) %>% dplyr::left_join(
    purrr::map_dfr(df, ~ pillar::type_sum(.x)) %>%
      tidyr::gather(key = "col_name", value = "starting_class"),
    by = "col_name"
  ) %>%
    # remove color column
    dplyr::filter(col_name %in% names(df))

  # Check if determined column classes are suitable
  correct <- compare_classes(column_info_df)
  column_info_df <- column_info_df %>% dplyr::left_join(correct, by = "col_name")

  return(column_info_df)
}


#' Check if determined column classes are suitable
#'
#' @param column_info_df dataframe of column names, determined_class, and starting_class
#'
#' @keywords internal
compare_classes <- function(column_info_df) {

  # Function to check for exceptions in class matching
  check_exceptions <- function(determined_class, starting_class){
    if (determined_class == starting_class) {
      return(TRUE)
    } else if(determined_class == "dbl" && starting_class == "int") {
      return(TRUE)
    } else if (determined_class == "time" && starting_class == "chr") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  purrr::pmap_dfr(column_info_df, function(col_name, determined_class, starting_class) {
    tibble::tibble(col_name = col_name, correct = check_exceptions(determined_class, starting_class))
  })
}
