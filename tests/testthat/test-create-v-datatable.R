path <- system.file("example-sdtm", package = "mrgda")


# used in tests
rows_per_id <- 3
num_ids <- 21

# Create a test dataframe
df <- tibble::tibble(
  age = sample(c(22,25,26,30,32,24), num_ids, replace = TRUE),
  Weight = rnorm(num_ids, 75, 15),
  sex = rep(c("M", "F"), num_ids)[1:num_ids],
  BLFL = rep(c(TRUE, FALSE), num_ids)[1:num_ids],
  DATETIME = rep(Sys.time(), num_ids),
  DATE = rep(Sys.Date(), num_ids),
  TIME = rep(format(as.POSIXct(Sys.time()), format = "%H:%M"), num_ids)
) %>%
  dplyr::mutate(
    ID = 1:n(),
    USUBJID = paste0("STUDY-1001-3053-", 1:n())
  ) %>%
  tidyr::uncount(rows_per_id) %>%
  dplyr::mutate(
    biomarker = rnorm(num_ids*rows_per_id, 5, 1),
  ) %>%
  dplyr::relocate(ID, USUBJID)

attr(df$USUBJID, "label") <- "Subject"


test_that("create_v_datatable correctly modifies dataframe", {

  # Test the function on the test dataframe
  result <- create_v_datatable(df, "USUBJID")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  expect_equal(ncol(result$x$data %>% dplyr::select(-color)), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$sex))
})


test_that("create_v_datatable works correctly for various .subject_col specifications", {

  ## No subject column
  result <- create_v_datatable(df %>% dplyr::select(-c("USUBJID", "ID")))
  expect_true(grepl("No subjects detected", result$x$caption, fixed = TRUE))


  ## Two subject columns found, use the first found
  result <- create_v_datatable(df)
  expect_true(grepl(paste("N Subjects (ID):", num_ids), result$x$caption, fixed = TRUE))
  result <- create_v_datatable(df %>% dplyr::relocate(USUBJID))
  expect_true(grepl(paste("N Subjects (USUBJID):", num_ids), result$x$caption, fixed = TRUE))
  result <- create_v_datatable(df, .subject_col = "USUBJID")
  expect_true(grepl(paste("N Subjects (USUBJID):", num_ids), result$x$caption, fixed = TRUE))

  ## Errors if multiple specified
  error_msg <- testthat::capture_error(create_v_datatable(df, .subject_col = c("USUBJID", "ID")))
  expect_equal(error_msg$message, "length(.subject_col) not equal to 1")
})


test_that("create_v_datatable formatting options work correctly", {
  result <- create_v_datatable(df %>% dplyr::relocate(sex))

  # Found subject column is relocated to front
  expect_equal(colnames(result$x$data)[1], "ID")

  # fixes/freezes ID column
  expect_equal(result$x$options$fixedColumns$leftColumns, 1)

  # make sure colors alternate with ID
  expect_equal(dplyr::n_distinct(result$x$data$color), 2)
  expect_equal(
    result$x$data$color,
    stats::ave(result$x$data$color, FUN = rep, each = rows_per_id)
  )

  # color column is hidden (column order starts at 0 for columnDefs)
  expect_equal(
    result$x$options$columnDefs[[4]],
    list(targets = ncol(df), visible = FALSE)
  )

  # column classes are correct
  column_info_df <- map_v_classes(df, result)
  expect_true(all(column_info_df$correct))
})


test_that("create_v_datatable works correctly for various .freeze_cols specifications", {

  result <- create_v_datatable(df)
  # fixes/freezes first ID column found by default (none specified)
  expect_equal(names(result$x$data)[1:result$x$options$fixedColumns$leftColumns], "ID")

  freeze_cols <- c("USUBJID", "biomarker")
  result <- create_v_datatable(df, .freeze_cols = freeze_cols)
  expect_equal(names(result$x$data)[1:result$x$options$fixedColumns$leftColumns], c("ID", freeze_cols))
})


# test_that("v spawns a background process for large dataframes", {
#   df <- haven::read_xpt(file.path(path, "lb.xpt"))
#
#   desired_rows <- 10000
#   df_large <- purrr::map_dfr(seq_len(ceiling(desired_rows / nrow(df))), ~ df)
#
#   # Needed for dev environment only
#   Sys.setenv('MRGDA_SHINY_DEV_LOAD_PATH' = here::here())
#
#   result <- v(df_large)
#   on.exit(result$kill())
#   expect_true(result$is_alive())
# })

test_that("create_v_datatable errors for large dataset when interactive", {
  df <- haven::read_xpt(file.path(path, "lb.xpt"))

  desired_rows <- 10000
  df_large <- purrr::map_dfr(seq_len(ceiling(desired_rows / nrow(df))), ~ df)

  rlang::with_interactive(value = TRUE, {
    error_msg <- testthat::capture_error(create_v_datatable(df_large))
  })

  expect_true(grepl(".df object size", unname(error_msg$message)))

  expect_equal(
    unname(error_msg$body),
    "Use `mrgda::src_viz(list(.df))` for large datasets, which renders the table using your R console"
  )
})
