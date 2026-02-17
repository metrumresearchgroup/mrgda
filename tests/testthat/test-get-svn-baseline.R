# Test 1: The function should return a list
test_that("get_svn_baseline output check: returns a list", {
  expect_true(inherits(get_svn_baseline("test.csv", FALSE), "list"))
})

# Test 2: The function should return NULL for base_df if the file does not exist
test_that("get_svn_baseline output check: returns NULL for base_df if the file does not exist", {
  result <- get_svn_baseline("nonexistent.csv", FALSE)
  expect_null(result$base_df)
})

# Test 3: The function should return FALSE for from_svn if .compare_from_svn is FALSE
test_that("get_svn_baseline output check: returns FALSE for from_svn if .compare_from_svn is FALSE", {
  result <- get_svn_baseline("test.csv", FALSE)
  expect_false(result$from_svn)
})

# Test 3b: svn_author and svn_date should be NA when not comparing from SVN
test_that("get_svn_baseline returns NA for svn_author and svn_date when not from SVN", {
  result <- get_svn_baseline("test.csv", FALSE)
  expect_true(is.na(result$svn_author))
  expect_true(is.na(result$svn_date))
})

# Test 4: The function should return a data frame for base_df if the file exists
test_that("get_svn_baseline output check: returns a data frame for base_df if the file exists", {
  # Create a test file
  write_csv_dots(
    x = data.frame(a = 1:5, b = 6:10),
    file = "test.csv"
  )
  on.exit(unlink("test.csv"))

  result <- get_svn_baseline("test.csv", FALSE)
  expect_s3_class(result$base_df, "data.frame")
})



# Test 5: The function should work with a custom reader and file extension
test_that("get_svn_baseline works with custom reader and file extension", {
  test_data <- list(a = 1, b = list(c = "hello"))
  yaml::write_yaml(test_data, "test.yml")
  on.exit(unlink("test.yml"))

  result <- get_svn_baseline("test.yml", FALSE, .reader = yaml::read_yaml, .file_ext = ".yml")
  expect_equal(result$base_df, test_data)
  expect_false(result$from_svn)
})

test_that("get_svn_baseline output check returns TRUE for from_svn if .compare_from_svn is TRUE and the file exists in SVN", {
  svn_dir1 <- local_svn_repo()
  withr::defer(unlink(svn_dir1, recursive = TRUE))

  system(paste0("rm -rf ", svn_dir1))
  system(paste0("svnadmin create ", svn_dir1))


  svn_dir2 <- local_svn_repo()
  withr::defer(unlink(svn_dir2, recursive = TRUE))

  withr::with_dir(svn_dir2, {

    system(paste0("svn co file:///", svn_dir1, " ", svn_dir2, " -q -q"))

    write_csv_dots(
      x = mtcars,
      file = "df.csv"
    )

    system("svn add df.csv -q -q")
    system("svn commit -m 'test' df.csv -q -q")

    result <- get_svn_baseline(file.path(svn_dir2, "df.csv"), TRUE)
    expect_true(result$from_svn)
    expect_true(all(result$base_df == mtcars))
    expect_false(is.na(result$svn_author))
    expect_false(is.na(result$svn_date))
  })


})
