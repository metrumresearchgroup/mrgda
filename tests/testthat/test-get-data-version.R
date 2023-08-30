test_lookup_path <- system.file("derived/endpoint-version-lookup.yaml", package = "mrgda")

test_that("get_data_version returns expected output", {
  test_endpoint <- "pk"

  test_output <- get_data_version(test_endpoint, test_lookup_path, .version = "current")
  test_output2 <- get_data_version(test_endpoint, test_lookup_path, .version = "previous")

  expect_true(inherits(test_output, "list"))
  expect_length(test_output, 2)

  # Check that $info and $file fields exist
  expect_true("info" %in% names(test_output))
  expect_true("file" %in% names(test_output))

  # Check that info is a dataframe and contains Major and Minor fields
  expect_true(inherits(test_output$info, "data.frame"))
  expect_true("Major" %in% names(test_output$info))
  expect_true("Minor" %in% names(test_output$info))
  expect_true(nrow(test_output$info) == 1)

  # Check that file is a character and contains the endpoint name, Major and Minor version
  expect_true(inherits(test_output$file, "character"))
  expect_true(grepl(test_endpoint, test_output$file))
  expect_true(grepl(paste0("-", test_output$info$Major, "-", test_output$info$Minor), test_output$file))

  # Explicit example testing
  expect_equal(test_output$info$Major, 1)
  expect_equal(test_output$info$Minor, 3)
  expect_equal(test_output$info$MasterVersion, "pk-1-2")
  expect_equal(test_output$file, "pk-1-3")

  # Explicit example testing
  expect_equal(test_output2$info$Major, 1)
  expect_equal(test_output2$info$Minor, 2)
  expect_equal(test_output2$info$MasterVersion, "pk-1-1")
  expect_equal(test_output2$file, "pk-1-2")
})
