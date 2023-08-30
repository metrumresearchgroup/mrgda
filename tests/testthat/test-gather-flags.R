nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE) %>% suppressMessages()
nm_spec_noflags <- yspec::ys_load(system.file("derived", "pk-noflags.yml", package = "mrgda"))
nm_spec_parflags <- yspec::ys_load(system.file("derived", "pk-partial_flags.yml", package = "mrgda"))
nm_spec_nocont <- yspec::ys_load(system.file("derived", "pk-missing-cont.yml", package = "mrgda"))
gather_results <- gather_flags(nm, nm_spec)

test_that("gather_flags standard case: Works with standard case", {
  expect_equal(c(gather_results$flags$evid, gather_results$flags$dvid), c("EVID", "DVID"))
  expect_identical(nm, gather_results$data)
  expect_identical(sort(gather_results$missing_flags$name), c("occ", "primary_keys", "tv_cat_cov"))
  expect_true(gather_results$missing_flags$Description[gather_results$missing_flags$name == "occ"] == "Occasion")
})

test_that("gather_flags special cases: Generates message if no flags specified", {
  expect_message(gather_flags(nm, nm_spec_noflags), "No mrgda specific flags found in spec file")
})
