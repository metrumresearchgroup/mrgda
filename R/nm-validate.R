#' Validate an NMTRAN dataset
#'
#'
#' @param .data a data frame
#' @param .spec a yspec object
#' @examples
#'
#' nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "nmvalidate"))
#'
#' nm <- readr::read_csv(system.file("derived", "pk.csv", package = "nmvalidate"), na = ".")
#'
#' nm_validate(nm, nm_spec)
#'
#' @md
#' @export
nm_validate <- function(.data, .spec, .error_on_fail = TRUE){

  tests_results <- list()

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


  # helper functions --------------------------------------------------------
  col_concat_nmvalidate <- function(.x){
    assertr::col_concat(data = .x, sep = "nmvalidate")
  }

  append_result <- function(.res, .list = tests_results) {

    res_se <-
      purrr::set_names(c("success", "errors")) %>%
      purrr::map(., ~ attr(.res, glue::glue("assertr_{.x}")))

    out <-
      if (!is.null(res_se$success)) {

        list(
          success = TRUE,
          description = gsub(" (critical)", "", unlist(res_se$success)[["description"]], fixed = TRUE),
          critical = grepl(" (critical)", unlist(res_se$success)[["description"]], fixed = TRUE)
        )

      } else if (!is.null(res_se$errors)) {

        list(
          success = FALSE,
          description = gsub(" (critical)", "", unlist(res_se$errors)[["description"]], fixed = TRUE),
          critical = grepl(" (critical)", unlist(res_se$errors)[["description"]], fixed = TRUE),
          error_content =
            .res %>%
            dplyr::slice(
              dplyr::pull(res_se$errors[[1]][["error_df"]], index)
            )
        )

      }


    c(.list, list(out))
  }

  # ##### BEGIN TESTS ##### -------------------------------------------------

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
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
        "No duplicates across: {paste(c(flags$id, flags$time, flags$primary_key), collapse = ', ')} (critical)"
      )
    ) %>%
    append_result


  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    dplyr::distinct(
      dplyr::across(
        c(
          flags$id,
          flags$bl_cov_cat,
          flags$bl_cov_cont
        )
      )
    ) %>%
    assertr::assert(
      assertr::is_uniq,
      flags$id,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "Duplicate baseline covariates (critical)"
    ) %>%
    append_result

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    assertr::assert(
      assertr::not_na,
      c(
        flags$id,
        flags$bl_cov_cat,
        flags$bl_cov_cont
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "NA baseline covariates (critical)"
    ) %>%
    append_result

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    assertr::assert(
      assertr::not_na,
      c(
        flags$id,
        flags$tv_cov_cat,
        flags$tv_cov_cont
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "NA time varying covariates"
    ) %>%
    append_result

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    dplyr::select(c(flags$study, flags$bl_cov_cont)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(flags$study)) %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(cols = flags$bl_cov_cont) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      ratio = (max(value) / min(value)) - 1
    ) %>%
    dplyr::ungroup() %>%
    assertr::assert(
      assertr::within_bounds(0, 0.5),
      ratio,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "Similar continuous baseline covariates across studies"
    ) %>%
    append_result


  class(tests_results) <- c("nm_validate_results", class(tests_results))

  # Return a true error of any critical failures
  critical_failures <-
    purrr::map_lgl(tests_results, ~ !.x$success & .x$critical) %>% sum

  if (critical_failures > 0 & .error_on_fail) {
    print(tests_results)
    stop("nm_validate found critical issues in data", call. = FALSE)
  }

  return(tests_results)
}


#' @method print nm_validate_results
#' @export
print.nm_validate_results <- function(.nm_validate_results) {

  cli::cli_h1("nm_validate() results:")

  num_passed <- purrr::map_lgl(.nm_validate_results, ~ .x$success) %>% sum
  num_non_critical_fail <- purrr::map_lgl(.nm_validate_results, ~ !.x$success & !.x$critical) %>% sum
  num_critical_fail <- purrr::map_lgl(.nm_validate_results, ~ !.x$success & .x$critical) %>% sum

  end_msg <- glue::glue("{num_passed} of {length(.nm_validate_results)} checks {crayon::green('PASSED')}")

  if (num_non_critical_fail > 0) {

    warn_msg <- glue::glue("{num_non_critical_fail} {crayon::yellow('WARNINGS')}")

    cli::cli_h2(glue::glue("Found {warn_msg}:"))

    end_msg <- glue::glue("{end_msg} ({warn_msg})")

    non_critical_failures <-
      purrr::map(.nm_validate_results, ~ {
        if(isTRUE(!.x$success & !.x$critical)) {
          return(.x)
        } else {
          return(NULL)
        }
      }) %>% purrr::compact()

    purrr::iwalk(non_critical_failures, function(res, i) {

      cat("\n")

      cli::cli_alert_warning("Warning {i} (Test {which(purrr::map(.nm_validate_results, ~.x$description) == res$description)}): {res$description} -- {nrow(res$error_content)} problem{?s}:")

      print(res$error_content)
    })

  }

  if (num_critical_fail > 0) {

    fail_msg <- glue::glue("{num_critical_fail} {crayon::red('FAILURES')}")

    cli::cli_h2(glue::glue("Found {fail_msg}:"))

    end_msg <- glue::glue("{end_msg} ({fail_msg})")

    critical_failures <-
      purrr::map(.nm_validate_results, ~ {
        if(isTRUE(!.x$success & .x$critical)) {
          return(.x)
        } else {
          return(NULL)
        }
      }) %>% purrr::compact()

    purrr::iwalk(critical_failures, function(res, i) {

      cat("\n")

      cli::cli_alert_danger("Failure {i} (Test {which(purrr::map(.nm_validate_results, ~.x$description) == res$description)}): {res$description} -- {nrow(res$error_content)} problem{?s}:")

      print(res$error_content)
    })

  }

  cli::cli_h1(end_msg)
}

