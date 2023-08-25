
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


# Create a test dataframe with necessary variability for testing
# v related functions
create_test_v_df <- function(rows_per_id = 3, num_ids = 21){
  df <- tibble::tibble(
    age = sample(c(22,25,26,30,32,24), num_ids, replace = TRUE),
    Weight = rnorm(num_ids, 75, 15),
    sex = rep(c("M", "F"), num_ids)[1:num_ids],
    BLFL = rep(c(TRUE, FALSE), num_ids)[1:num_ids],
    DATETIME = rep(Sys.time(), num_ids),
    DATE = rep(Sys.Date(), num_ids),
    TIME = rep(format(as.POSIXct(Sys.time()), format = "%H:%M"), num_ids)
  ) %>%
    dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:n())
    ) %>%
    tidyr::uncount(rows_per_id) %>%
    dplyr::mutate(
      biomarker = rnorm(num_ids*rows_per_id, 5, 1),
    ) %>%
    dplyr::relocate(ID, USUBJID)

  attr(df$USUBJID, "label") <- "Subject"

  return(df)
}


#' Extract column names, labels, and classes from header html
#'
#' @param df A starting dataframe
#'
#' @keywords internal
extract_v_headers <- function(df){
  headers <- format_v_headers(df)

  html <- rvest::read_html(paste0(headers, collapse = ""))
  columns <- rvest::html_elements(html, "h4")
  col_names <- rvest::html_text(columns)

  col_styles <- rvest::html_elements(html, "span")
  col_styles <- rvest::html_text(col_styles)

  col_labels <- col_styles[seq(1, length(col_styles), by = 2)]
  col_types <- col_styles[seq(2, length(col_styles), by = 2)] %>%
    stringr::str_extract("(?<=<).*?(?=>)")


  tibble::tibble(
    col_name = col_names,
    col_label = col_labels,
    col_type = col_types
  )
}


with_bg_env <- function(code){
  Sys.setenv("MRGDA_SHINY_DEV_LOAD_PATH" = "")
  # Dont run if inside an R CMD Check environment (package is installed)
  if(!testthat::is_checking()){
    Sys.setenv("MRGDA_SHINY_DEV_LOAD_PATH" = here::here())
    Sys.setenv("RSTUDIO" = 1)
    on.exit(Sys.setenv("MRGDA_SHINY_DEV_LOAD_PATH" = ""))
  }

  result <- eval(code)
  return(result)
}

# Make sure it's not set when tests are run
Sys.setenv("MRGDA_SHINY_DEV_LOAD_PATH" = "")
