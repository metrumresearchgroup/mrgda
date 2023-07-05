
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
