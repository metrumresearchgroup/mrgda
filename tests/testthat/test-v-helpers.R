path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path) %>% suppressMessages()

# used in tests
rows_per_id <- 3
num_ids <- 21

# Create a test dataframe
df <- create_test_v_df(rows_per_id, num_ids)


test_that("create_v_datatable correctly modifies dataframe", {
  skip_if_v_missing_deps()

  # Test the function on the test dataframe
  result <- create_v_datatable(df, "USUBJID")

  # Check that the output is a datatables object
  expect_true(inherits(result, "datatables"))

  # Check that the output has the correct number of rows
  expect_equal(nrow(result$x$data), nrow(df))

  # Check that the output has the correct number of columns
  cols_remove <- c("bg_color", "ft_color", "subj_border")
  expect_equal(ncol(result$x$data %>% dplyr::select(-dplyr::all_of(cols_remove))), ncol(df))

  # Check that columns with less than 20 unique values are converted to factors
  expect_true(is.factor(result$x$data$sex))
})


test_that("create_v_datatable works correctly for various .subject_col specifications", {
  skip_if_v_missing_deps()

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
  skip_if_v_missing_deps()

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

})


test_that("create_v_datatable works correctly for various .freeze_cols specifications", {
  skip_if_v_missing_deps()

  result <- create_v_datatable(df)
  # fixes/freezes first ID column found by default (none specified)
  expect_equal(names(result$x$data)[1:result$x$options$fixedColumns$leftColumns], "ID")

  freeze_cols <- c("USUBJID", "biomarker")
  result <- create_v_datatable(df, .freeze_cols = freeze_cols)
  expect_equal(names(result$x$data)[1:result$x$options$fixedColumns$leftColumns], c("ID", freeze_cols))
})




test_that("create_v_datatable errors for large dataset when interactive", {
  skip_if_v_missing_deps()

  df_lb <- src_list$lb

  desired_rows <- 10000
  df_large <- purrr::map_dfr(seq_len(ceiling(desired_rows / nrow(df_lb))), ~ df_lb)

  rlang::with_interactive(value = TRUE, {
    error_msg <- testthat::capture_error(create_v_datatable(df_large))
  })

  expect_true(grepl(".df object size", unname(error_msg$message)))

  expect_equal(
    unname(error_msg$body),
    "Use `mrgda::v(.df)` for large datasets, which renders the table using your R console"
  )
})

test_that("format_v_headers creates a header column correctly", {
  skip_if_v_missing_deps()

  df_lb <- src_list$lb

  # Extract column names and class information
  header_tbl <- extract_v_headers(df_lb)

  # Function for class mapping - these discrepancies are OK
  compare_classes <- function(col_type, expected_col_type){
    if (col_type == expected_col_type) {
      return(TRUE)
    } else if(col_type == "dbl" && expected_col_type == "int") {
      return(TRUE)
    } else if (col_type == "time" && expected_col_type == "chr") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Determine expected classes (how tibble sets it)
  column_info_df <- header_tbl %>%
    dplyr::left_join(
      purrr::map_dfr(df_lb, ~ pillar::type_sum(.x)) %>%
        tidyr::gather(key = "col_name", value = "expected_col_type"),
      by = "col_name"
    ) %>%
    dplyr::mutate(
      correct = purrr::map2_lgl(col_type, expected_col_type, ~ compare_classes(.x, .y))
    )

  # Check if determined column classes are suitable
  expect_true(all(column_info_df$correct))

  # Check labels
  col_labels <- purrr::map_chr(colnames(df_lb), ~ {attr(df_lb[[.x]], "label")})
  expect_true(all(column_info_df$col_label == col_labels))
})

test_that("gather_v_cols determines the correct subject and frozen columns", {
  skip_if_v_missing_deps()

  get_freeze_cols <- function(df_list){
    unique_cols <- names(table(unlist(purrr::map(df_list, ~ names(.x)))))
    unique_cols[!(unique_cols %in% c("USUBJID", "ID"))]
  }

  # `USUBJID` is most common ID column
  df_list1 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
  )

  # `ID` is most common ID column
  df_list2 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      ID = 1:n()
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
  )

  # USUBJID and ID are both present, use USUBJID
  df_list3 <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    c = create_test_v_df(rows_per_id = 3, num_ids = 21)
  )

  expect_message(
    v_cols <- gather_v_cols(df_list1),
    "Detected Subject Column: USUBJID"
  )
  expect_equal(v_cols$.subject_col, "USUBJID")

  expect_message(
    v_cols <- gather_v_cols(df_list2),
    "Detected Subject Column: ID"
  )
  expect_equal(v_cols$.subject_col, "ID")

  expect_message(
    v_cols <- gather_v_cols(df_list3),
    "Detected Subject Column: USUBJID"
  )
  expect_equal(v_cols$.subject_col, "USUBJID")


  # Subtitle and freeze calls are the same for all example lists - test once
  expect_equal(v_cols$.freeze_cols$subtitle, paste0("(", c(rep("c", 8), "a", "b"), ")"))
  expect_equal(
    v_cols$.freeze_cols$col_name,
    get_freeze_cols(df_list3)
  )

  # Subject column not found
  error_msg <- testthat::capture_error(gather_v_cols(df_list1, .subject_col = "hello"))
  expect_equal(error_msg$message, ".subject_col (hello) is not present in any dataframe")
})


test_that("create_global_filter creates the correct UI", {
  skip_if_v_missing_deps()

  filter_ui <- create_global_filter(.subject_col = NULL)
  expect_equal(filter_ui, shiny::div())

  id_col <- "USUBJID"
  filter_ui <- create_global_filter(.subject_col = id_col)
  expect_true(grepl(id_col, as.character(filter_ui)))
})


test_that("make_v_caption works correctly for various .subject_col specifications", {
  skip_if_v_missing_deps()

  df <- create_test_v_df(rows_per_id = 3, num_ids = 21)
  cap <- make_v_caption("test", df)
  expect_equal(cap$name, "test")
  expect_equal(cap$n_subs, 0)
  expect_equal(cap$label, "test No subjects detected")

  cap <- make_v_caption("test", df, .subject_col = "USUBJID")
  expect_equal(cap$n_subs, 21)
  expect_equal(cap$label, "test (N USUBJID: 21)")

  cap <- make_v_caption("test", df, .subject_col = "ID")
  expect_equal(cap$n_subs, 21)
  expect_equal(cap$label, "test (N ID: 21)")
})



test_that("filter_v_subject filters using global subject filter", {
  skip_if_v_missing_deps()

  df <- create_test_v_df(rows_per_id = 3, num_ids = 21)

  # Test filtering
  filter_df <- filter_v_subject(df, "USUBJID", "3053-4")
  expect_equal(nrow(filter_df), 3)
  expect_true(all(grepl("3053-4", filter_df$USUBJID)))

  # Test if none found
  filter_df <- filter_v_subject(df, "USUBJID", "3053-100")
  expect_equal(nrow(filter_df), 1)
  expect_true(grepl("No subjects found", filter_df$USUBJID))
  expect_true(
    filter_df %>% dplyr::select(-"USUBJID") %>% t() %>%
      data.frame() %>% unlist() %>% unique() %>% is.na()
  )
})


test_that("find_df_with_col finds the correct dataframes", {

  expect_error(
    find_df_with_col("ID", list(df)),
    ".df_list must be a named list"
  )

  args <- setup_v_list(src_list)
  df_list <- args$.df_list

  # None found
  cols_df <- find_df_with_col("ID", df_list)
  expect_equal(cols_df$n_domain, 0)
  expect_equal(cols_df$subtitle, "()")

  # Dataframes found
  cols_df <- find_df_with_col("STUDYID", df_list)
  expect_equal(cols_df$n_domain, 7)
  expect_equal(cols_df$subtitle, "(ae, dm and 5 more)")

  # Multiple columns (how the function is called in gather_v_cols)
  cols <- c("STUDYID", "VISIT", "AEDECOD")
  cols_df <- purrr::map_dfr(cols, find_df_with_col, df_list)
  expect_equal(cols_df$col_name, cols)
  expect_equal(cols_df$n_domain, c(7,4,1))
  expect_equal(cols_df$subtitle, c("(ae, dm and 5 more)", "(eg, lb and 2 more)", "(ae)"))
})


test_that("setup_v_list works correctly (integration test)", {

  expected_subj_col <- "USUBJID"

  expect_message(
    args <- setup_v_list(src_list),
    glue::glue("Detected Subject Column: {expected_subj_col}")
  )

  # Attributes
  expect_equal(names(args), c(".df_list", ".subject_col", ".freeze_cols"))

  # Expected column
  expect_equal(args$.subject_col, expected_subj_col)

  # Filtering of mrgda specific dataframes
  expect_equal(
    sort(setdiff(names(src_list), names(args$.df_list))),
    c("mrgda_labels", "mrgda_src_meta")
  )

  # found subject column should not be in subject columns
  freeze_cols <- args$.freeze_cols
  expect_false(expected_subj_col %in% freeze_cols$col_name)

  # All other unique column names should be present
  all_col_names <- purrr::map(args$.df_list, names) %>% purrr::list_c() %>% unique()
  all_col_names <- all_col_names[-grep(expected_subj_col, all_col_names)]
  expect_true(rlang::is_empty(setdiff(freeze_cols$col_name, all_col_names)))
  expect_true(rlang::is_empty(setdiff(all_col_names, freeze_cols$col_name)))

})
