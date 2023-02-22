path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(path)

# Add descriptions to domain level
attr(src_list$ae, "label") = "Adverse Events"
attr(src_list$dm, "label") = "Demographics"
attr(src_list$lb, "label") = "Labs"
attr(src_list$mh, "label") = "Medical History"

# Add descriptions to columns
attr(src_list$dm$RACE, "label") = "Subject Race"
attr(src_list$dm$SEX, "label") = "Subject Sex"

# View source data labels
src_dir_labels <- view_src_data_labels(src_list)

test_that("view_src_data_labels outputs description of source data domains [NMV-SDL-001]", {
  expect_equal(src_dir_labels$DOMAIN_LABEL[src_dir_labels$DOMAIN_NAME == "ae"][1], "Adverse Events")
  expect_equal(src_dir_labels$DOMAIN_LABEL[src_dir_labels$DOMAIN_NAME == "dm"][1], "Demographics")
  expect_equal(src_dir_labels$DOMAIN_LABEL[src_dir_labels$DOMAIN_NAME == "lb"][1], "Labs")
  expect_equal(src_dir_labels$DOMAIN_LABEL[src_dir_labels$DOMAIN_NAME == "mh"][1], "Medical History")
  expect_true(length(unique(src_dir_labels$DOMAIN_LABEL[src_dir_labels$DOMAIN_NAME == "mh"])) == 1)
})

test_that("view_src_data_labels outputs description of column names [NMV-SDL-002]", {
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "RACE"], "Subject Race")
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "SEX"], "Subject Sex")
  expect_equal(src_dir_labels$COLUMN_LABEL[src_dir_labels$COLUMN_NAME == "EGSTRESN"], "Missing")
})
