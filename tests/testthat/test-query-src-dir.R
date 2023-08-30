path <- system.file("example-sdtm", package = "mrgda")

test_that("query_src_dir returns all domain and column combinations with a matching string", {
  query_race <- query_src_dir(.src_directory = path, .string = "RACE") %>% suppressMessages()
  expect_identical(
    query_race$MATCHING_ROWS[query_race$COLUMN == "LBSTNRC" & query_race$MATCH == "NEGATIVE TO TRACE"],
    "19, 20, 38, 58, 59, 80, 81"
  )
  expect_false(any(query_race$DOMAIN == "ae"))
  expect_error(query_src_dir(path, "stringnotfound") %>% suppressMessages())
})

test_that("query_src_dir is not case sensitive", {
  expect_identical(
    query_src_dir(.src_directory = path, .string = "RACE") %>% suppressMessages(),
    query_src_dir(.src_directory = path, .string = "race") %>% suppressMessages()
  )
})
