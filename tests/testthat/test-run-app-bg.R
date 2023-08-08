
testthat::test_that("fmt_bg_stderr formats messages correctly", {

  msgs <- c("Loading: package","loading package", "package is loading (bullet point)",
            "","Warning: some warning", "continuation of warning message (bullet point)", "",
            "error: some error", "Error some error", " error space in the front") %>%
    paste(collapse="\n")

  # Write messages to file
  iodir <- tempfile(pattern = "mrgda--")
  dir.create(iodir); on.exit(unlink(iodir, recursive = TRUE))
  stderr_file <- file.path(iodir, "stderr")
  readr::write_file(msgs, stderr_file)

  fmt_msgs <- fmt_bg_file(bg_file = stderr_file)

  # Run cli::cli_bullets(fmt_msgs) for manual inspection
  expect_equal(names(fmt_msgs), c("v", "v", ">", " ", "!", ">", " ", "x", "x", "x"))
})
