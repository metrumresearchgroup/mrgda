nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

.temp_csv <- tempfile(fileext = ".csv")
.temp_script <- tempfile(fileext = ".R")
writeLines(text = "2 + 2", con = .temp_script)

nm_write(.data = nm, .spec = nm_spec, .file = .temp_csv, .this_script = .temp_script)
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

test_that("nm_write works with special characters in file name [NMV-NMW-003]", {
  .temp_csv <- tempfile(fileext = "-pk.csv")
  nm_write(.data = nm, .spec = nm_spec, .file = .temp_csv, .this_script = .temp_script)
  expect_equal(nm, readr::read_csv(.temp_csv, na = "."))
})

test_that("nm_write writes out script to source script directory [NMV-NMW-004]", {
  expect_true(file.exists(.temp_script))
})
