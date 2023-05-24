# Test that check_columns correctly identifies correctly formatted data
test_that("check_columns correctly identifies correctly formatted data", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  lookup <- data.frame(column_name = c("A", "B", "C"),
                       type = c("required", "required", "permitted"),
                       alt_name = c("AA", "BB", "CC"))

  expect_true(all(check_columns(df, lookup)))
})

# Test that check_columns correctly identifies missing required column
test_that("check_columns correctly identifies missing required column", {
  df <- data.frame(A = 1:5, C = 11:15)
  lookup <- data.frame(column_name = c("A", "B", "C"),
                       type = c("required", "required", "permitted"),
                       alt_name = c("AA", "BB", "CC"))

  result <- check_columns(df, lookup)
  expect_false(result["All required columns present"])
})

# Test that check_columns correctly identifies unpermitted column
test_that("check_columns correctly identifies unpermitted column", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15, D = 16:20)
  lookup <- data.frame(column_name = c("A", "B", "C"),
                       type = c("required", "required", "permitted"),
                       alt_name = c("AA", "BB", "CC"))

  result <- check_columns(df, lookup)
  expect_false(result["No unpermitted columns present"])
})

# Test that check_columns correctly identifies and suggests rename for alternative column name
test_that("check_columns correctly identifies and suggests rename for alternative column name", {
  df <- data.frame(AA = 1:5, BB = 6:10, C = 11:15)
  lookup <- data.frame(column_name = c("A", "B", "C"),
                       type = c("required", "required", "permitted"),
                       alt_name = c("AA", "BB", "CC"))

  result <- check_columns(df, lookup)

  temp <- capture.output(result <- check_columns(df, lookup))

  expect_equal(temp, "rename(A = AA, B = BB)")
})
