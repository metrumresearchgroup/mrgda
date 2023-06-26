test_that("src_viz input validation", {
  # Test with non-list input
  non_list_input <- data.frame(a = 1:5, b = 6:10)
  expect_error(src_viz(non_list_input), "The input must be a list of data frames.")
})

test_that("src_viz app structure", {
  # Prepare a proper input list
  df_list <- list(a = data.frame(x = 1:5), b = data.frame(y = 6:10))

  # Run the function and get the UI
  app <- src_viz(df_list)

  expect_true(inherits(app, "shiny.appobj"))
  expect_true(app$options$launch.browser)

})

