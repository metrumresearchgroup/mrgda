path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path) %>% suppressMessages()

# Global inputs
input_lst <- list(
  data_select = "ae",
  subject_filter = "CDISC01.100008",
  freeze_cols = "AGE",
  show_filters = FALSE,
  dt_options = c("show_labels", "trunc_labels"),
  trunc_length = 20,
  digits = 3,
  ft_size = 9,
  subj_contrast = FALSE
)

# Mimics reactiveValues set in v_global_server
global_var_lst <- list(
  data_select = src_list[[shiny::req(input_lst$data_select)]],
  subject_filter = input_lst$subject_filter,
  freeze_col_choices = input_lst$freeze_cols,
  dt_args = list(
    .show_filters = input_lst$show_filters,
    .show_labels = "show_labels" %in% input_lst$dt_options,
    .wrap_labels = "wrap_labels" %in% input_lst$dt_options,
    .trunc_labels = "trunc_labels" %in% input_lst$dt_options,
    .trunc_length = input_lst$trunc_length,
    .digits = input_lst$digits,
    .ft_size = input_lst$ft_size,
    .subj_contrast = input_lst$subj_contrast
  )
)

# Function for setting all inputs
# Has to be a function outside of testServer
# looping/purrr does not work within testServer
set_all_inputs <- function(input_lst, session){
  purrr::iwalk(input_lst, function(.x, .y){
    session$setInputs(!!sym(.y) := .x)
  })
}

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

  expect_false(nzchar(Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")))
  app <- v_shiny_internal(df_list, dont_run = TRUE)

  expect_true(inherits(app, "shiny.appobj"))
})


test_that("v spawns a background process", {
  expect_false(nzchar(Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")))
  # Needed for dev environment only
  expect_message(
    result <- with_bg_env(v(src_list)),
    "Detected Subject Column: USUBJID"
  )
  result$wait(1)
  expect_true(result$is_alive())
  result$kill()
  expect_false(result$is_alive())
  expect_false(nzchar(Sys.getenv("MRGDA_SHINY_DEV_LOAD_PATH")))
  gc()
})



test_that("v_global_server sets all inputs in correct format", {

  # Wrap v_global_server in template server since it behaves like a normal function
  test_server <- function(input, output, session) {
    # Note: This function is not a module (no namespace)
    global_vars <- v_global_server(.df_list = src_list, input, output)
  }


  # Test that global_vars() is identical to `global_var_lst`
  testServer(test_server, {
    # Set all inputs to input_lst
    set_inputs(input_lst, session)
    session$flushReact()
    expect_true(rlang::is_empty(setdiff(global_vars(), global_var_lst)))
  })

})

test_that("v_mod_server works as expected", {
  global_vars <- reactive(global_var_lst)

  # Base case with filter and no freeze columns found
  shiny::testServer(
    v_mod_server,
    args = list(.subject_col = "USUBJID", global_vars = global_vars),{
      # Test filtering
      expect_equal(nrow(data()), 3)
      # Test subject column
      expect_equal(subject_col_df(), "USUBJID")
      # No freeze columns found (AGE is only in dm domain)
      expect_true(is.null(freeze_cols()))
    })

  # No subject column specified
  shiny::testServer(
    v_mod_server,
    args = list(.subject_col = NULL, global_vars = global_vars),{
      # Test filtering
      expect_equal(nrow(data()), 16)
      # Test subject column
      expect_equal(subject_col_df(), NULL)
    })

  # Subject column not present; with freeze columns
  global_var_lst2 <- global_var_lst
  global_var_lst2$freeze_col_choices <- c("AEENDTC", "AEDECOD", "AGE")
  global_vars2 <- reactive(global_var_lst2)

  shiny::testServer(
    v_mod_server,
    args = list(.subject_col = "ID", global_vars = global_vars2), {
      # Test filtering
      expect_equal(nrow(data()), 16)
      # Test subject column
      expect_equal(subject_col_df(), NULL)
      # Freeze columns - AGE is filtered out
      expect_equal(freeze_cols(), c("AEENDTC", "AEDECOD"))
    })
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
