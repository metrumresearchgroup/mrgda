#' Validate an NMTRAN dataset
#'
#' @description
#' This function is intended to perform a series of checks on a derived data
#' set by leveraging the data specification used during assembly. If a check
#' fails, the function will output a description of the problem and code to
#' use for debugging.
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @param .error_on_fail if `TRUE`, an R error is executed upon failures
#' @examples
#'
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
#'
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
#'
#' nm_validate(nm, nm_spec)
#'
#' @md
#' @export
nm_validate <- function(.data, .spec, .error_on_fail = TRUE){

  # Check inputs
  stopifnot(".data must be a data.frame" = inherits(.data, "data.frame"))
  stopifnot(".spec must be a yspec object" = inherits(.spec, "yspec"))
  stopifnot(".error_on_fail must be a logical" = is.logical(.error_on_fail))

  # argument names ----------------------------------------------------------
  arg_names <- as.list(as.character(match.call())[-1])
  names(arg_names) <- names(match.call())[-1]

  # grab data and spec flags ------------------------------------------------

  tests_results <- list()

  gather_return <- gather_flags(.data, .spec)

  flags <- gather_return$flags

  # helper functions --------------------------------------------------------

  collapse_covs <- function(.covs){
    paste(.covs, collapse = ", ")
  }

  pass_fail <- function(.code) {

    .ans <- list()

    # Run code uses input data
    .code_run <-
      gsub("{arg_names$.data}", ".data", .code, fixed = TRUE) %>%
      glue::glue_collapse(, sep = " %>% ") %>%
      glue::glue()

    .code_return <-
      glue::glue_collapse(.code, sep = " %>% ") %>%
      glue::glue()

    .outputdf <- rlang::parse_expr(.code_run) %>% rlang::eval_tidy()

    .ans$debug <- .code_return
    .ans$success <- nrow(.outputdf) == 0

    return(.ans)

  }

  # ##### BEGIN TESTS ##### -------------------------------------------------

  tests_results <- list()

  # Duplicate primary keys  -------------------------------------------------
  tests_results[["No duplicate primary keys"]] <-
    pass_fail(
      .code = c(
        "{arg_names$.data}",
        "dplyr::select({collapse_covs(c(flags$id, flags$time, flags$primary_key))})",
        "dplyr::count({collapse_covs(c(flags$id, flags$time, flags$primary_key))})",
        "dplyr::filter(n > 1)"
      )
    )

  # Non-unique baseline covariates ------------------------------------------
  tests_results[["Non-unique baseline covariates"]] <-
    pass_fail(
      .code = c(
        "{arg_names$.data}",
        "dplyr::select({collapse_covs(c(flags$id, flags$bl_cat_cov, flags$bl_cont_cov))})",
        "dplyr::filter(complete.cases(.))",
        "dplyr::distinct()",
        "dplyr::group_by(across({flags$id}))",
        "dplyr::add_count()",
        "dplyr::ungroup()",
        "dplyr::filter(n > 1)"
      )
    )

  # Missing baseline covariates ---------------------------------------------

  tests_results[["No missing covariates"]] <-
    pass_fail(
      .code = c(
        "{arg_names$.data}",
        "dplyr::select({collapse_covs(c(flags$id, flags$bl_cat_cov, flags$bl_cont_cov, flags$tv_cont_cov, flags$tv_cat_cov))})",
        "dplyr::filter(!complete.cases(.))"
      )
    )

  # Output ------------------------------------------------------------------
  class(tests_results) <- c("nm_validate_results", class(tests_results))

  # Return a true error of any failures
  failures <- purrr::map_lgl(tests_results, ~ !.x$success) %>% sum

  if (failures > 0 & .error_on_fail) {
    print(tests_results)
    message("\n")
    stop("nm_validate() found issues", call. = FALSE)
  }

  return(tests_results)

}


#' @method print nm_validate_results
#' @export
print.nm_validate_results <- function(x, ...) {

  cli::cli_h1("nm_validate() results:")

  num_passed <- purrr::map_lgl(x, ~ .x$success) %>% sum
  num_fail <- purrr::map_lgl(x, ~ !.x$success) %>% sum

  end_msg <- glue::glue("{num_passed} of {length(x)} checks {crayon::green('PASSED')}")

  if (num_passed > 0) {

    passes <-
      purrr::map(x, ~ {
        if(isTRUE(.x$success)) {
          return(.x)
        } else {
          return(NULL)
        }
      }) %>% purrr::compact()

    for (i in 1:length(passes)) {

      cat("\n")

      cli::cli_alert_success("{names(passes)[i]}")
    }

  }

  if (num_fail > 0) {

    failures <-
      purrr::map(x, ~ {
        if(isTRUE(!.x$success)) {
          return(.x)
        } else {
          return(NULL)
        }
      }) %>% purrr::compact()

    for (i in 1:length(failures)) {

      cat("\n")

      cli::cli_alert_danger("{names(failures)[i]} -- Copy/paste and run the following code:")

      cat(gsub("%>%", "%>%\n", as.character(failures[[i]]$debug, fixed = TRUE)))
      cat("\n")
    }

  }

  # end_msg)
  cat("\n")
  cat(summary_line(n_fail = num_fail, n_pass = num_passed))

}
