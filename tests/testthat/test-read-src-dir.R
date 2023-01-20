path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path)

test_that("read_src_dir ae domain: check data is read in correctly [NMV-RSD-001]", {
  expect_equal(src_list$ae, haven::read_xpt(file.path(path, "ae.xpt")))
})

test_that("read_src_dir provides count of subjects in each domain [NMV-RSD-002]", {
  expect_true(src_list$usubjid$ae[1])
  expect_equal(src_list$usubjid$eg[5], FALSE)
})

