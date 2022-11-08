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

  # ##### BEGIN TESTS ##### -------------------------------------------------

  tests_results <- list()

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
  tests_results[["2"]] <-
    gather_return$data %>%
    dplyr::distinct(
      dplyr::across(
        c(
          flags$id,
          flags$bl_cat_cov,
          flags$bl_cont_cov
        )
      )
    ) %>%
    assertr::assert(
      assertr::is_uniq,
      flags$id,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "Non-unique baseline covariates"
    ) %>%
    build_result()

  # -------------------------------------------------------------------------
  tests_results[["3"]] <-
    gather_return$data %>%
    assertr::assert(
      assertr::not_na,
      c(
        flags$id,
        flags$bl_cat_cov,
        flags$bl_cont_cov
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "Missing baseline covariates"
    ) %>%
    build_result()

  # -------------------------------------------------------------------------
  tests_results[["4"]] <-
    gather_return$data %>%
    assertr::assert(
      assertr::not_na,
      c(
        flags$id,
        flags$tv_cat_cov,
        flags$tv_cont_cov
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "Missing time varying covariates"
    ) %>%
    build_result()

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

