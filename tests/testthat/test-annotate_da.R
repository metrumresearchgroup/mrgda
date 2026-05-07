test_that("annotate_da function works correctly", {
  # Capture the output of the annotate_da function
  output <- capture.output(annotate_da('Test Explanation', {
    54 * 78
  }))
  expect_equal("[1] 4212", output)
})

test_that("use_annotate_da_file clears file and annotate_da appends output", {
  withr::with_tempdir({
    annotation_file <- "script/data-assembly/study-101-annotations.md"
    fs::dir_create(fs::path_dir(annotation_file))
    writeLines("old output", annotation_file)
    withr::local_options(mrgda.annotate_da_file = NULL)

    result <- use_annotate_da_file(annotation_file)

    expect_equal(result, annotation_file)
    expect_equal(getOption("mrgda.annotate_da_file"), annotation_file)
    expect_equal(readLines(annotation_file), character())

    capture.output(annotate_da("ID = 1 has outlier, will remove", {
      data.frame(ID = 1, DV = 100)
    }))

    saved <- readLines(annotation_file)
    expect_false(any(saved == "old output"))
    expect_true(any(saved == "# Explanation: ID = 1 has outlier, will remove"))
    expect_true(any(grepl("ID.*DV", saved)))
    expect_true(any(grepl("1.*100", saved)))
  })
})

test_that("annotate_da restores tibble print options", {
  withr::local_options(
    tibble.print_max = 10,
    tibble.print_min = 5,
    tibble.width = 80
  )

  capture.output(annotate_da("Test Explanation", {
    tibble::tibble(x = 1:20)
  }))

  expect_equal(getOption("tibble.print_max"), 10)
  expect_equal(getOption("tibble.print_min"), 5)
  expect_equal(getOption("tibble.width"), 80)
})

test_that("use_annotate_da_file requires configured output directory to exist", {
  withr::with_tempdir({
    expect_error(
      use_annotate_da_file("script/data-assembly/study-101-annotations.md"),
      "directory does not exist"
    )
  })
})
