nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
results <- view_mrgda_flags(.data = nm, .spec = nm_spec, .view = FALSE)

test_that("view_mrgda_flags concatenates multiple columns when more than 1 present for a flag[ [NMV-VSF-002]", {
  expect_true(results$`Assigned Columns`[results$Flag == "bl_cont_cov"] == "WTBL, BMIBL, AGEBL")
  expect_true(results$`Assigned Columns`[results$Flag == "bl_cat_cov"] == "SEX, RACE")
  expect_true(results$`Assigned Columns`[results$Flag == "id"] == "ID")
})
