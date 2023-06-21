#' Run all SDTM checks
#'
#' @param .src_list List of all soure domain data.frames
#' @param .subject_col Column name of subject identifier (character)
#'
#' @export
run_all_sdtm_checks <- function(.src_list, .subject_col = "USUBJID") {

  domain_lookup <- get_sdtm_lookup(.subject_col)
  return_list <- list()

  for (domain.i in names(.src_list)) {

    if (!(domain.i %in% domain_lookup$DOMAIN)) {
      next
    }
    return_list[[domain.i]] <- list()

    return_list[[domain.i]][["Duplicates"]] <- sdtm_check_duplicates(.domain_df = src_list[[domain.i]], .domain_name = domain.i, .subject_col = .subject_col)
    return_list[[domain.i]][["MissingDatetimes"]] <- sdtm_check_missing_datetime(.domain_df = src_list[[domain.i]], .domain_name = domain.i, .subject_col = .subject_col)

  }

  test_results <- list()

  for (domain.j in names(return_list)) {

    test_results[[domain.j]] <- dplyr::tibble()

    tests <- names(return_list[[domain.j]])

    for (test.i in tests) {

      result <- return_list[[domain.j]][[test.i]]

      test_results[[domain.j]] <-
        dplyr::bind_rows(
          test_results[[domain.j]],
          dplyr::tibble(
            Domain = domain.j,
            Test = result$Name,
            Result = result$Result,
            pct_RecordsFail = ifelse(result$Result == "Fail", result$PctRecordsFail, "N/A")
          )
        )

    }
  }

  return(test_results)
}


