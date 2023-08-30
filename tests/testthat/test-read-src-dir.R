path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path) %>% suppressMessages()

test_that("read_src_dir ae domain: check data is read in correctly", {
  expect_equal(src_list$ae, haven::read_xpt(file.path(path, "ae.xpt")) %>% suppressMessages())
  expect_equal(src_list$dm, haven::read_xpt(file.path(path, "dm.xpt")) %>% suppressMessages())
  expect_equal(src_list$lb, haven::read_xpt(file.path(path, "lb.xpt")) %>% suppressMessages())
  expect_equal(src_list$vs, haven::read_xpt(file.path(path, "vs.xpt")) %>% suppressMessages())
})

test_that("read_src_dir allows user to only load in specific domains", {
  src_list <- read_src_dir(.path = path, .read_domains = c("dm", "lb")) %>% suppressMessages()
  expect_equal(length(src_list), 4)
})

test_that("read_src_dir outputs a dataframe with labels from each domain", {
  expect_true(all(c("ae", "dm", "lb", "vs") %in% unique(src_list$mrgda_labels$DOMAIN)))
  expect_equal(names(src_list$mrgda_labels), c("DOMAIN", "COLUMN_NAME", "COLUMN_LABEL"))
})

test_that("read_src_dir works with a directory containing sas7bdat files", {
  dir <- local_svn_repo()
  withr::defer(unlink(dir, recursive = TRUE))
  haven::write_xpt(mtcars, path = file.path(dir, "mtcars.xpt"))
  haven::write_xpt(Theoph, path = file.path(dir, "theoph.xpt"), label = NULL)
  src_list2 <- read_src_dir(dir, .file_types = "xpt")

  expect_true(length(src_list2) == 4)
  expect_true(all(names(src_list2) %in% c("mtcars", "theoph", "mrgda_labels", "mrgda_src_meta")))
  expect_true(!is.null(src_list2$mtcars))
  expect_true(!is.null(src_list2$theoph))
  expect_equal(src_list2$theoph, haven::read_xpt(file.path(dir, "theoph.xpt")) %>% suppressMessages())

})

test_that("read_src_dir works with a directory containing csv files", {
  dir <- system.file("derived", package = "mrgda")
  src_list2 <- read_src_dir(dir, .file_types = "csv")

  expect_true(length(src_list2) == 4)
  expect_true(all(names(src_list2) %in% c("pk-errors", "pk", "mrgda_labels", "mrgda_src_meta")))
  expect_true(!is.null(src_list2$pk))
  expect_equal(src_list2$pk, readr::read_csv(system.file("derived/pk.csv", package = "mrgda")) %>% suppressMessages())

})

