nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

.temp_csv <- tempfile(fileext = ".csv")
.temp_script <- tempfile(fileext = ".R")
writeLines(text = "2 + 2", con = .temp_script)

write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv)
.csv_in <- readr::read_csv(.temp_csv, na = ".")
.xpt_in_name <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),
                       "/",
                       gsub(".csv", ".xpt", basename(.temp_csv), fixed = TRUE))
.xpt_in <- haven::read_xpt(.xpt_in_name)
.xpt_in_labels <- purrr::map(.xpt_in, ~ attr(.x, "label"))

# Baseline continuous covariates tests ------------------------------------

test_that("write_derived write csv: csv is written correctly and matches data [NMV-NMW-001]", {
  expect_equal(nm, .csv_in)
})

test_that("write_derived write xpt: xpt data includes correct labels [NMV-NMW-002]", {
  expect_equal(
    yspec::ys_get_short(nm_spec)[order(names(yspec::ys_get_short(nm_spec)))],
    .xpt_in_labels[order(names(.xpt_in_labels))]
  )
})

test_that("write_derived works with special characters in file name [NMV-NMW-003]", {
  .temp_csv <- tempfile(fileext = "-pk.csv")
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv)
  expect_equal(nm, readr::read_csv(.temp_csv, na = "."))
})

test_that("write_derived writes out script to source script directory [NMV-NMW-004]", {
  expect_true(file.exists(.temp_script))
})