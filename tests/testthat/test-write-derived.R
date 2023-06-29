nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

.temp_csv <- tempfile(fileext = ".csv")

write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv) %>% suppressMessages()
.csv_in <- readr::read_csv(.temp_csv, na = ".") %>% suppressMessages()
.xpt_in_name <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),
                       "/",
                       gsub(".csv", ".xpt", basename(.temp_csv), fixed = TRUE))
.xpt_in <- haven::read_xpt(.xpt_in_name) %>% suppressMessages()
.xpt_in_labels <- purrr::map(.xpt_in, ~ attr(.x, "label"))


test_that("write_derived write csv: csv is written correctly and matches data [NMV-NMW-001]", {
  expect_equal(nm, .csv_in)
})

test_that("write_derived write xpt: xpt data includes correct labels [NMV-NMW-002]", {
  expect_equal(
    yspec::ys_get_short(nm_spec)[order(names(yspec::ys_get_short(nm_spec)))],
    .xpt_in_labels[order(names(.xpt_in_labels))]
  )
})

test_that("subject level data gets written out correctly", {

  .subject_level <-
    file.path(tools::file_path_sans_ext(.temp_csv), "subject-level.csv") %>%
    readr::read_csv() %>%
    suppressMessages()

  .subject_level_cols <- c("ID", unique(.subject_level$Column))

  .distinct_subject_cols <- names(mrgda::distinct_subject_columns(nm, "ID"))

  expect_true(all(sort(.distinct_subject_cols) == sort(.subject_level_cols)))
})


test_that("write_derived works with special characters in file name [NMV-NMW-003]", {
  .temp_csv <- tempfile(fileext = "-pk.csv")
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv) %>% suppressMessages()
  expect_equal(nm, readr::read_csv(.temp_csv, na = ".") %>% suppressMessages())
})

