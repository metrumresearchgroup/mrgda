nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

.temp_csv <- tempfile(fileext = ".csv")

nm_write(nm, nm_spec, .temp_csv)
.csv_in <- readr::read_csv(.temp_csv, na = ".")
.xpt_in_name <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),
                       "/",
                       gsub(".csv", ".xpt", basename(.temp_csv), fixed = TRUE))
.xpt_in <- haven::read_xpt(.xpt_in_name)
.xpt_in_labels <- purrr::map(.xpt_in, ~ attr(.x, "label"))

# Baseline continuous covariates tests ------------------------------------

test_that("nm_write write csv: csv is written correctly and matches data [NMV-NMW-001]", {
  expect_equal(nm, .csv_in)
})

test_that("nm_write write xpt: xpt data includes correct labels [NMV-NMW-002]", {
  expect_equal(
    yspec::ys_get_short(nm_spec)[order(names(yspec::ys_get_short(nm_spec)))],
    .xpt_in_labels[order(names(.xpt_in_labels))]
  )
})
