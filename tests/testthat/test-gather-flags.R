nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
nm_spec_noflags <- yspec::ys_load(system.file("derived", "pk-noflags.yml", package = "mrgda"))
nm_spec_parflags <- yspec::ys_load(system.file("derived", "pk-partial_flags.yml", package = "mrgda"))
nm_spec_nocont <- yspec::ys_load(system.file("derived", "pk-missing-cont.yml", package = "mrgda"))


test_that("gather_flags standard case: Works with standard case [NMV-GAT-001]", {
  gather_results <- gather_flags(nm, nm_spec)
  expect_equal(gather_results$flags$primary_keys, c("EVID", "DVID"))
})

test_that("gather_flags special cases: Creates a new column for every missing flag [NMV-GAT-002]", {
  gather_results <- gather_flags(nm, nm_spec)
  expect_equal(gather_results$data$mrgda_tv_cov_cat[1], "tv_cov_cat flag missing")
})

test_that("gather_flags special cases: Fills in missing flags in spec [NMV-GAT-002]", {
  gather_results <- gather_flags(nm, nm_spec)
  expect_equal(gather_results$flags$tv_cov_cat, "mrgda_tv_cov_cat")
})

test_that("gather_flags special cases: Generates error if no flags specified [NMV-GAT-002]", {
  expect_error(gather_flags(nm, nm_spec_noflags), "No flags found in spec file")
})

test_that("gather_flags special cases: Fills numerical column if continuous missing [NMV-GAT-002]", {
  gather_results <- gather_flags(nm, nm_spec_nocont)
  expect_equal(gather_results$data$mrgda_tv_cov_cont[1], 0)
})

