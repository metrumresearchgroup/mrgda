test_that("visualize_data validates input paths", {

  expect_error(
    visualize_data("does-not-exist.csv"),
    "`.csv_path` does not exist"
  )

  csv_path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), csv_path)

  expect_error(
    visualize_data(csv_path, "missing-spec.yaml"),
    "`.spec_path` does not exist"
  )
})

test_that("visualize_data returns a shiny app with options", {

  csv_path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), csv_path)

  app <- visualize_data(csv_path)

  expect_true(inherits(app, "shiny.appobj"))
  expect_equal(app$options$.csv_path, csv_path)
  expect_null(app$options$.spec_path)
  expect_true(isTRUE(app$options$launch.browser))
  expect_true(isTRUE(app$options$quiet))
})
