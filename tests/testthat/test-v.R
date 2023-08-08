path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path) %>% suppressMessages()

test_that("v_shiny_internal app structure", {
  # Prepare a proper input list
  df_list <- list(
    a = data.frame(x = 1:5) %>% dplyr::mutate(
      ID = 1:n(),
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    ),
    b = data.frame(y = 6:10) %>% dplyr::mutate(
      USUBJID = paste0("STUDY-1001-3053-", 1:dplyr::n())
    )
  )

  app <- v_shiny_internal(df_list, dont_run = TRUE)

  expect_true(inherits(app, "shiny.appobj"))

  error_msg <- testthat::capture_error(v_shiny_internal(df_list, .subject_col = "hello"))
  expect_equal(error_msg$message, ".subject_col (hello) is not present in any dataframe")
})

test_that("v spawns a background process", {
  expect_false(nzchar(Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")))
  # Needed for dev environment only
  result <- with_bg_env(v(src_list))
  # result <- v(src_list)
  result$wait(1)
  expect_true(result$is_alive())
  result$kill()
  expect_false(result$is_alive())
  expect_false(nzchar(Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")))
  gc()
})


# This test is necessary, and caught a few bugs
# test_that("v_shiny_internal works when no subject columns are found", {
#   non_list_input <- data.frame(a = 1:5, b = 6:10)
#   app <- v_shiny_internal(non_list_input, dont_run = TRUE)
#
#   # This test passes either way (not an actual test)
#   # TODO: write shiny test for no subjects
#   expect_error(app, NA)
# })
