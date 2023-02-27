path <- system.file("example-sdtm", package = "mrgda")

test_that("query_src_dir returns all domain and column combinations with a matching string []", {
  query_race <- query_src_dir(.src_directory = path, .string = "RACE")
  expect_true(query_race$DOMAIN[query_race$TYPE == "label"] == "dm")
  expect_true(nrow(query_race) == 6)
})
