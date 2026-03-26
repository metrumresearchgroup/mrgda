test_that("annotate_da function works correctly", {
  # Capture the output of the annotate_da function
  output <- capture.output(annotate_da('Test Explanation', {
    54 * 78
  }))
  expect_equal("[1] 4212", output)
})
