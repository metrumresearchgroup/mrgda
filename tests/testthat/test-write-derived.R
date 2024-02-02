nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

withr::with_tempdir({
  writeLines("Version: 1.0", con = "temp.Rproj")
  svn_dir <- local_svn_repo()
  withr::defer(unlink(svn_dir, recursive = TRUE))

  .temp_csv <- tempfile(fileext = ".csv", tmpdir = svn_dir)
  .temp_script <- tempfile(fileext = ".R", tmpdir = svn_dir)
  writeLines(text = "2 + 2", con = .temp_script)

  .temp_csv <- tempfile(fileext = ".csv")


  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  # Run again to create a diff
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  .csv_in <- readr::read_csv(.temp_csv, na = ".") %>% suppressMessages()
  .xpt_in_name <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),
                         "/",
                         gsub(".csv", ".xpt", basename(.temp_csv), fixed = TRUE))
  .xpt_in <- haven::read_xpt(.xpt_in_name) %>% suppressMessages()
  .xpt_in_labels <- purrr::map(.xpt_in, ~ attr(.x, "label"))
  .history <- paste0(gsub(".csv", "", .temp_csv, fixed = TRUE),"/history.csv")
})

test_that("write_derived write csv: csv is written correctly and matches data", {
  expect_equal(nm, .csv_in)
})

test_that("write_derived write xpt: xpt data includes correct labels", {
  expect_equal(
    yspec::ys_get_short(nm_spec)[order(names(yspec::ys_get_short(nm_spec)))],
    .xpt_in_labels[order(names(.xpt_in_labels))]
  )
})


test_that("write_derived makes a blank diff file when no diffs", {
  diffs <- read.csv(paste0(gsub(".csv", "" , .temp_csv, fixed=TRUE), "/diffs.csv"))
  expect_true(
    all(names(diffs) == c("name", "value"))
  )
})


test_that("write_derived works with special characters in file name", {
  .temp_csv <- tempfile(fileext = "-pk.csv")
  write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE) %>% suppressMessages()
  expect_equal(nm, readr::read_csv(.temp_csv, na = ".") %>% suppressMessages())
})

test_that("write_derived outputs history csv in meta data folder", {
  expect_true(file.exists(.history))
})

test_that("write_derived gives error if data has comma in any value", {
  df_comma <-
    dplyr::tibble(
      name = c("smith, john", "james", "mark"),
      values = c(1, 2, 3)
      )

  expect_error(write_derived(.data = df_comma, .spec = nm_spec, .file = .temp_csv, .compare_from_svn = FALSE))
})

test_that("write_derived works with no ID column in data", {
  nm$ID <- NULL
  nm_spec$ID <- NULL
  .temp_csv2 <- paste0(tempfile(), ".csv")

  expect_error(write_derived(.data = nm, .spec = nm_spec, .file = .temp_csv2, .compare_from_svn = FALSE))
})

test_that("if yspec check fails  write derived does not continue", {
  nm_spec$ADAC <- NULL
  expect_error(write_derived(.data = nm, .spec = nm_spec, .file =  paste0(tempfile(), ".csv"), .compare_from_svn = FALSE))
})


