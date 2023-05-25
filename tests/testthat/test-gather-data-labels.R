path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(path) %>% suppressMessages()



# View source data labels
src_dir_labels <- gather_data_labels(src_list)

test_that("gather_data_labels outputs description of column names [NMV-SDL-001]", {
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "RACE"], "RACE_LABEL")
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "SEX"], "SEX_LABEL")
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "EGSTRESN"], "EGSTRESN_LABEL")
})
