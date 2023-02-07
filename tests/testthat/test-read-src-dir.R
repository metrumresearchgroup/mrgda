path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path)

test_that("read_src_dir ae domain: check data is read in correctly [NMV-RSD-001]", {
  expect_equal(src_list$ae, haven::read_xpt(file.path(path, "ae.xpt")))
})

test_that("read_src_dir provides count of subjects in each domain [NMV-RSD-002]", {
  expect_true(src_list$usubjid$ae[1])
  expect_equal(src_list$usubjid$eg[5], FALSE)
})

path2 <- system.file("example-sdtm-no-subject", package = "mrgda")
src_list2 <- read_src_dir(.path = path2)

test_that("read_src_dir can handle src dir with all missing USUBJID [NMV-RSD-003]", {
  expect_true(is.null(src_list2$usubjid))
})

test_that("read_src_dir allows user to only load in specific domains [NMV-RSD-004]", {
  src_list <- read_src_dir(.path = path, .read_domains = c("dm", "lb"))
  expect_equal(length(src_list), 3)
  expect_true(ncol(src_list$usubjid) == 3)
})
