path <- system.file("example-sdtm", package = "mrgda")


# used in tests
rows_per_id <- 3
num_ids <- 21

# Create a test dataframe
df <- create_test_v_df(rows_per_id, num_ids)
cols_remove <- c("bg_color", "ft_color", "subj_border")

test_that("create_v_datatable correctly modifies dataframe", {

  # Test the function on the test dataframe
  result <- create_v_datatable(df, "USUBJID")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  expect_equal(ncol(result$x$data %>% dplyr::select(-dplyr::all_of(cols_remove))), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$sex))
})


test_that("create_v_datatable works correctly for various .subject_col specifications", {

  ## No subject column, none specified - no relocation or color coding
  result <- create_v_datatable(df %>% dplyr::select(-c("USUBJID", "ID")))
  expect_equal(as.character(unique(result$x$data$bg_color)), "white")

  ## Two subject columns found, use the first found
  result <- create_v_datatable(df)
  expect_equal(names(result$x$data)[1], "ID")
  result <- create_v_datatable(df %>% dplyr::relocate(USUBJID))
  expect_equal(names(result$x$data)[1], "USUBJID")

  # user specification takes precedence
  result <- create_v_datatable(df, .subject_col = "USUBJID")
  expect_equal(names(result$x$data)[1], "USUBJID")

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
  expect_equal(dplyr::n_distinct(result$x$data$bg_color), 2)
  expect_equal(
    result$x$data$bg_color,
    stats::ave(result$x$data$bg_color, FUN = rep, each = rows_per_id)
  )

  # color column is hidden (column order starts at 0 for columnDefs)
  col_defs <- result$x$options$columnDefs
  hidden_cols_opt <- which(purrr::map_lgl(col_defs, ~"visible" %in% names(.x)))
  expect_equal(
    col_defs[[5]],
    list(targets = seq(ncol(df), length.out = 3), visible = FALSE)
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
    "Use `mrgda::v(.df)` for large datasets, which renders the table using your R console"
  )
})
