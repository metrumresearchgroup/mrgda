# Test 1: The function should return a list
test_that("get_base_df returns a list", {
  expect_true(inherits(get_base_df("test.csv", FALSE), "list"))
})

# Test 2: The function should return NULL for base_df if the file does not exist
test_that("get_base_df returns NULL for base_df if the file does not exist", {
  result <- get_base_df("nonexistent.csv", FALSE)
  expect_null(result$base_df)
})

# Test 3: The function should return FALSE for from_svn if .compare_from_svn is FALSE
test_that("get_base_df returns FALSE for from_svn if .compare_from_svn is FALSE", {
  result <- get_base_df("test.csv", FALSE)
  expect_false(result$from_svn)
})

# Test 4: The function should return a data frame for base_df if the file exists
test_that("get_base_df returns a data frame for base_df if the file exists", {
  # Create a test file
  readr::write_csv(data.frame(a = 1:5, b = 6:10), "test.csv")
  on.exit(unlink("test.csv"))

  result <- get_base_df("test.csv", FALSE)
  expect_s3_class(result$base_df, "data.frame")
})


test_that("get_base_df returns TRUE for from_svn if .compare_from_svn is TRUE and the file exists in SVN", {
  svn_dir1 <- tempdir()

  system(paste0("rm -rf ", svn_dir1))
  system(paste0("svnadmin create ", svn_dir1))

  svn_dir2 <- tempdir()

  system(paste0("svn co file:///", svn_dir1, " ", svn_dir2))
  setwd(svn_dir2)
  write.csv(mtcars, "df.csv", row.names = FALSE)
  system("svn add df.csv")
  system("svn commit -m 'test' df.csv")

  result <- get_base_df(file.path(svn_dir2, "df.csv"), TRUE)
  expect_true(result$from_svn)
  expect_true(all(result$base_df == mtcars))
})
