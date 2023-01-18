path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path)

test_that("read_src_dir ae domain: check data is read in correctly", {
  expect_equal(src_list$ae.xpt, haven::read_xpt(file.path(path, "ae.xpt")))
})

