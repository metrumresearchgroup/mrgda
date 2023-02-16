path <- system.file("example-sdtm", package = "mrgda")
src_summary <- view_src_dir_summary(.path = path)

test_that("src_dir_summary summarizes all domains and outputs file sizes [NMV-SSD-001]", {
  expect_true(all(c("ae", "dm", "eg", "lb", "mh", "pe", "vs") %in% src_summary$DOMAIN))
  expect_equal(src_summary$SIZE_KB[src_summary$DOMAIN == "pe"], 12.8)
  expect_equal(src_summary$SIZE_KB[src_summary$DOMAIN == "eg"], 14.16)
})

test_that("src_dir_summary provides descriptions of domains [NMV-SSD-002]", {
  expect_equal(src_summary$DOMAIN_NAME[src_summary$DOMAIN == "mh"], "Medical History")
})
