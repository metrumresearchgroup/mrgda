# Load in spec and data
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".", show_col_types = FALSE)

# Write out subject level data
write_subj_level(.data = nm, .subject_identifier = "ID", file.path(tempdir(), "test.Rds"))
subj_lvl <- readRDS(file.path(tempdir(), "test.Rds"))

test_that("write_subj_level only outputs columns with 1 unique value per subject [NMV-WSL-001]", {
  expect_true(!("EVID" %in% names(subj_lvl)))
  expect_true("RACE" %in% names(subj_lvl))
  expect_equal(nrow(subj_lvl), nrow(nm %>% dplyr::group_by(ID) %>% dplyr::distinct(RACE)))
  expect_true(file.exists(file.path(tempdir(), "test.Rds")))
})

