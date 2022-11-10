#' Validate an NMTRAN dataset
#'
#' @description
#' This function is intended to perform a series of checks on a derived data
#' set by leveraging the data specification used during assembly. If a check
#' fails, the function will output a description of the problem and where it
#' occurs in the dataset.
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

  # argument names ----------------------------------------------------------
  args_used <- as.character(sys.calls()[[1]])[-1]

  arg_names <- list()

  for(i in 1:length(args_used)){

    arg.i = args_used[i]

    check.i <- purrr::set_names(c(".data", ".spec")) %>%
      purrr::map(., ~ identical(get(.x), get(arg.i))) %>%
      unlist()

    arg_names[[names(check.i[check.i])]] <- arg.i

  }

  .data_name <- arg_names$.data
  .spec_name <- arg_names$.spec

  # grab data and spec flags ------------------------------------------------

  tests_results <- list()

  gather_return <- gather_flags(.data, .spec)

  flags <- gather_return$flags

  # helper functions --------------------------------------------------------
  col_concat_nmvalidate <- function(.x){
    assertr::col_concat(data = .x, sep = "nmvalidate")
  }

  build_result <- function(.res, .list = tests_results) {

    res_se <-
      purrr::set_names(c("success", "errors")) %>%
      purrr::map(., ~ attr(.res, glue::glue("assertr_{.x}")))

    out <-
      if (!is.null(res_se$success)) {

        list(
          success = TRUE,
          description = unlist(res_se$success)[["description"]]
        )

      } else if (!is.null(res_se$errors)) {

        list(
          success = FALSE,
          description = unlist(res_se$errors)[["description"]],
          error_content =
            .res %>%
            dplyr::slice(
              dplyr::pull(res_se$errors[[1]][["error_df"]], index)
            )
        )

      }

  }

  collapse_covs <- function(.covs){
    paste(.covs, collapse = ", ")
  }

  pass_fail <- function(.description, .code) {

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

    .ans$description <- .description
    .ans$debug <- .code_return
    .ans$result <- if (nrow(.outputdf) > 0) {
      "Passed"} else {"Failed"}



  }

  # ##### BEGIN TESTS ##### -------------------------------------------------

  tests_results <- list()
  browser()

  # -------------------------------------------------------------------------
  tests_results[["1"]] <-
    gather_return$data %>%
    assertr::assert_rows(
      col_concat_nmvalidate,
      assertr::is_uniq,
      c(
        flags$id,
        flags$time,
        flags$primary_key
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = glue::glue(
        "No duplicates across: {paste(c(flags$id, flags$time, flags$primary_key), collapse = ', ')}"
      )
    ) %>%
    build_result()


  # -------------------------------------------------------------------------
  tests_results[["2"]] <- pass_fail(
    .description = "Non-unique baseline covariates",
    .code = c(
      "{arg_names$.data}",
      "dplyr::select({collapse_covs(c(flags$id, flags$bl_cat_cov, flags$bl_cont_cov))})",
      "dplyr::distinct()",
      "dplyr::count(flags$id)",
      "dplyr::filter(n > 1)"
    )
  )

  # Missing baseline covariates ---------------------------------------------

  tests_results[["3"]] <- pass_fail(
    .description = "Missing baseline covariates",
    .code = c(
      "{arg_names$.data}",
      "dplyr::select({collapse_covs(c(flags$id, flags$bl_cat_cov, flags$bl_cont_cov))})",
      "dplyr::filter(!complete.cases(.))"
    )
  )

  # Missing time varying covariates -----------------------------------------

  tests_results[["4"]] <- pass_fail(
    .description = "Missing time varying covariates",
    .code = c(
      "{arg_names$.data}",
      "dplyr::select({collapse_covs(c(flags$id, flags$tv_cont_cov))})",
      "dplyr::filter(!complete.cases(.))"
    )
  )

  # Output ------------------------------------------------------------------
  class(tests_results) <- c("nm_validate_results", class(tests_results))

  # Return a true error of any failures
  failures <- purrr::map_lgl(tests_results, ~ !.x$success) %>% sum

  if (failures > 0 & .error_on_fail) {
    print(tests_results)
    stop("nm_validate found issues in data", call. = FALSE)
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

  if (num_fail > 0) {

    fail_msg <- glue::glue("{num_fail} {crayon::red('FAILURES')}")

    cli::cli_h2(glue::glue("Found {fail_msg}:"))

    end_msg <- glue::glue("{end_msg} ({fail_msg})")

    failures <-
      purrr::map(x, ~ {
        if(isTRUE(!.x$success)) {
          return(.x)
        } else {
          return(NULL)
        }
      }) %>% purrr::compact()

    purrr::iwalk(failures, function(res, i) {

      cat("\n")

      cli::cli_alert_danger("Test {i} failure: {res$description} -- {nrow(res$error_content)} problem{?s}:")

      print(res$error_content)
    })

  }

  cli::cli_h1(end_msg)
}


