
nm_validate <- function(.data, .spec){

  tests_results <- list()

  # gather flags ------------------------------------------------------------
  recognized_flags <-
    c(
      "id",
      "study",
      "primary_keys",
      "tafd",
      "bl_cov_cat",
      "bl_cov_cont",
      "tv_cov_cat",
      "tv_cov_cont"
    )

  flags <-
    purrr::set_names(recognized_flags) %>%
    purrr::map(
      ~ tryCatch(
        yspec::ys_filter(
          x = .spec,
          expr =  rlang::eval_bare(rlang::parse_expr(.x))
        ) %>%
          dplyr::as_tibble() %>%
          dplyr::pull(name),
        error = function(e) {
          NULL
        }
      )
    )

  # helper function ---------------------------------------------------------
  append_result <- function(.res, .list = tests_results) {

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


    c(.list, list(out))
  }

  # ##### BEGIN TESTS ##### -------------------------------------------------

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    assertr::assert_rows(
      assertr::col_concat,
      assertr::is_uniq,
      c(
        .flags$id,
        .flags$tafd,
        .flags$primary_key
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "No duplicates across id, tafd, and primary keys"
    ) %>%
    append_result


  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    dplyr::distinct(
      dplyr::across(
        c(
          .flags$id,
          .flags$bl_cov_cat,
          .flags$bl_cov_cont
        )
      )
    ) %>%
    assertr::assert(
      assertr::is_uniq,
      .flags$id,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "no duplicate subject level covariates"
    ) %>%
    append_result

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    assertr::assert(
      assertr::not_na,
      c(
        .flags$id,
        .flags$bl_cov_cat,
        .flags$bl_cov_cont,
        .flags$tv_cov_cat,
        .flags$tv_cov_cont
      ),
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      description = "no na covariates"
    ) %>%
    append_result

  # -------------------------------------------------------------------------
  tests_results <-
    .data %>%
    dplyr::select(c(.flags$study, .flags$bl_cov_cont)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(.flags$study)) %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(cols = .flags$bl_cov_cont) %>%
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
      description = "similar continuous covariates across studies"
    ) %>%
    append_result


  class(tests_results) <- c("nm_validate_results", class(tests_results))

  return(tests_results)
}



print.nm_validate_results <- function(.nm_validate_results) {

  cli::cli_h1("nm_validate() results:")

  num_passed <- purrr::map_lgl(.nm_validate_results, ~ .x$success) %>% sum

  end_msg <- glue::glue("{num_passed} of {length(.nm_validate_results)} checks {crayon::green('PASSED')}")

  if (num_passed != length(.nm_validate_results)) {

    fail_msg <- glue("{length(.nm_validate_results) - num_passed} {crayon::red('FAILURES')}")

    cli::cli_h2(glue::glue("Found {fail_msg}:"))

    end_msg <- glue::glue("{end_msg} ({fail_msg})")

    failures <-
      purrr::map(.nm_validate_results, ~ {
        if(isTRUE(.x$success)) {
          return(NULL)
        } else {
          return(.x)
        }
      }) %>% purrr::compact()

    purrr::iwalk(failures, function(res, i) {

      cat("\n")

      cli::cli_alert_danger("Failure {i}: {res$description} -- {nrow(res$error_content)} problem{?s}:")

      print(res$error_content)
    })
  }

  cli::cli_h1(end_msg)
}

nm_validate(nm_errors, nm_spec)
