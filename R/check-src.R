#' Run all source data checks
#'
#' @description
#' This will run all source data checks available in the `mrgda` package. When
#' given a list of source data.frames it will perform a series of checks on each.
#' A high level summary of the passing/failing checks will be provided in the console
#' and more in depth results can be accessed by setting the `.return_details` argument to
#' `TRUE`.
#'
#' @param .src_list List of all soure domain data.frames
#' @param .subject_col Column name of subject identifier (character)
#' @param .return_details Return list of all summary checks be returned (TRUE/FALSE)
#'
#' @export
check_src <- function(.src_list, .subject_col = "USUBJID", .return_details = FALSE) {

  return_list <- list()

  for (domain.i in names(.src_list)) {

    domain_lookup <- get_sdtm_lookup(domain.i)
    if (is.null(domain_lookup)) {
      next
    }
    return_list[[domain.i]] <- list()

    return_list[[domain.i]][["Duplicates"]] <-
      check_src_duplicates(
        .domain_df = .src_list[[domain.i]],
        .domain_name = domain.i,
        .subject_col = .subject_col
      )

    return_list[[domain.i]][["MissingDatetimes"]] <-
      check_src_missing_datetime(
        .domain_df = .src_list[[domain.i]],
        .domain_name = domain.i,
        .subject_col = .subject_col
      )

  }

  test_results <- dplyr::tibble()

  for (domain.j in names(return_list)) {

    tests.i <- names(return_list[[domain.j]])

    for (test.j in tests.i) {

      result.j <- return_list[[domain.j]][[test.j]][c("Name", "ColsCheck", "Result", "N fail (%)")]
      result.j <- result.j[!is.na(names(result.j))]

      test_results <-
        dplyr::bind_rows(
          test_results,
          purrr::map_dfr(result.j, ~ paste(.x, collapse = ", "))
        )
    }
  }

  print(
    cli::boxx(
      padding = 0,
      knitr::kable(
        x = test_results,
        align = 'c',
        format = "simple"
      )
    )
  )


  if(.return_details) {
    return(return_list)
  } else {
    return(invisible(NULL))
  }
}


