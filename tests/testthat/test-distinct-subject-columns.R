# Load in spec and data
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

subj_lvl <- distinct_subject_columns(.data = nm, .subject_col = "ID")

test_that("write_subj_level only outputs columns with 1 unique value per subject [NMV-WSL-001]", {
  expect_true(!("EVID" %in% names(subj_lvl)))
  expect_true("RACE" %in% names(subj_lvl))
  expect_equal(nrow(subj_lvl), nrow(nm %>% dplyr::group_by(ID) %>% dplyr::distinct(RACE)))
  expect_identical(
    subj_lvl %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise_all(~length(unique(.x))) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(-ID) %>%
      dplyr::distinct(value) %>%
      dplyr::pull(value),
    1L
  )
})

