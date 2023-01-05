nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)
nm_spec_noflags <- yspec::ys_load(system.file("derived", "pk-noflags.yml", package = "mrgda"))
nm_spec_parflags <- yspec::ys_load(system.file("derived", "pk-partial_flags.yml", package = "mrgda"))
nm_spec_nocont <- yspec::ys_load(system.file("derived", "pk-missing-cont.yml", package = "mrgda"))


test_that("gather_flags standard case: Works with standard case [NMV-GAT-001]", {
  gather_results <- gather_flags(nm, nm_spec)
  expect_equal(c(gather_results$flags$evid, gather_results$flags$dvid), c("EVID", "DVID"))
})

test_that("gather_flags special cases: Generates error if no flags specified [NMV-GAT-002]", {
  expect_message(gather_flags(nm, nm_spec_noflags), "No flags found in spec file")
})

