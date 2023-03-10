nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
results <- view_spec_flags(.data = nm, .spec = nm_spec)

test_that("view_spec_flags doesn't detect columns that do not exist in spec or data", {
  expect_true(results$`Detected Columns`[results$`Flag name` == "primary_keys"] == "")
  expect_true(results$`Detected Columns`[results$`Flag name` == "tv_cat_cov"] == "")
  expect_true(results$`Detected Columns`[results$`Flag name` == "occ"] == "")
})

test_that("view_spec_flags concatenates multiple columns when more than 1 present for a flag", {
  expect_true(results$`Detected Columns`[results$`Flag name` == "bl_cont_cov"] == "WTBL, BMIBL, AGEBL")
  expect_true(results$`Detected Columns`[results$`Flag name` == "bl_cat_cov"] == "SEX, RACE")
})
