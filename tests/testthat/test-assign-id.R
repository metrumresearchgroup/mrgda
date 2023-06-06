test_that("assign_id function works correctly [NMV-AID-001]", {

  # Generate a sample dataframe for the tests
  .data <- data.frame(USUBJID = c("A", "B", "C"))
  .new_data_file <- paste0(tempfile(), ".csv")

  expect_true(nrow(assign_id(.data, .previously_derived_path = .new_data_file)) == 3) %>% suppressMessages()
  expect_true(ncol(assign_id(.data, .previously_derived_path = .new_data_file)) == 2) %>% suppressMessages()

  expect_true(nrow(assign_id(.data)) == 3) %>% suppressMessages()
  expect_true(ncol(assign_id(.data)) == 2) %>% suppressMessages()

  write.csv(assign_id(.data), .new_data_file)


  expect_true(nrow(assign_id(.data, .previously_derived_path = .new_data_file)) == 3) %>% suppressMessages()
  expect_true(ncol(assign_id(.data, .previously_derived_path = .new_data_file)) == 2) %>% suppressMessages()

  # Test 1: Testing whether the function runs without error
  test_that("Function runs without error", {
    expect_error(assign_id(.data, .new_data_file, .subject_col = "USUBJID"), NA) %>% suppressMessages()
  })

  # Test 2: Testing the function when the "ID" column already exists
  .data_with_id <- .data
  .data_with_id$ID <- c(1, 2, 3)
  test_that("Function correctly detects presence of 'ID' column", {
    expect_error(assign_id(.data_with_id, .new_data_file, .subject_col = "USUBJID"), "Data already contains ID") %>% suppressMessages()
  })

  # Test 3: Testing the function when the subject column doesn't exist
  .data_without_subject <- .data
  names(.data_without_subject)[1] <- "SUBJ"
  test_that("Function correctly detects absence of subject column", {
    expect_error(assign_id(.data_without_subject, .new_data_file, .subject_col = "USUBJID"), "Subject column not found in data")
  })

  # Test 5: Testing that the function adds new IDs correctly
  .data_new_subject <- data.frame(USUBJID = c("A", "B", "C", "D", "E"))
  test_that("Function correctly adds new subjects", {
    df_with_new_ids <- assign_id(.data_new_subject, .new_data_file, .subject_col = "USUBJID") %>% suppressMessages()
    expect_equal(df_with_new_ids$ID, c(1, 2, 3, 4, 5))
  })


  test_that("assign_id adds new subjects and provides unique ID", {

    # Generate a sample dataframe for the tests
    data <- data.frame(USUBJID = c("A", "B", "C", "D"))
    lookup_path <- paste0(tempfile(), ".csv")

    data_w_id <-
      assign_id(.data = data, .new_data_file, .subject_col = "USUBJID") %>% suppressMessages()

    expect_equal(ncol(data_w_id), 2)
    expect_equal(nrow(data_w_id), 4)
    expect_true(c("ID") %in% names(data_w_id))
    expect_true(c("USUBJID") %in% names(data_w_id))

    data <- data.frame(USUBJID = c("A", "B", "G", "D", "E", "F", "C"))

    data_w_id2 <-
      assign_id(.data = data, .new_data_file, .subject_col = "USUBJID") %>% suppressMessages()

    expect_true(data_w_id2$ID[data_w_id2$USUBJID == "C"] == 3)
    expect_true(nrow(data_w_id2) == 7)
  })

  test_that("assign_id throws error if previous data set doesn't have ID or subject column", {

    data <- data.frame(USUBJID = c("A", "B", "C", "D"))
    lookup_path <- paste0(tempfile(), ".csv")
    write.csv(data, lookup_path)

    expect_error(assign_id(.data = data, .previously_derived_path = lookup_path), "ID column not found in previous data")
    expect_error(assign_id(.data = data, .subject_col = "USUBJID2"), "Subject column not found in data")
  })

})


