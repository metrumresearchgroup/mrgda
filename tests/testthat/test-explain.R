test_that("explain function works correctly", {
  # Capture the output of the explain function
  output <- capture.output(explain('Test Explanation', {
    54 * 78
  }))
  expect_equal("[1] 4212", output)
})
