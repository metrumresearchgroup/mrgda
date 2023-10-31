path <- system.file("example-sdtm", package = "mrgda")

test_that("query_src_dir returns all domain and column combinations with a matching string", {
  query_white <- query_src_dir(.src_directory = path, .string = "WHITE") %>% suppressMessages()
  query_race <- query_src_dir(.src_directory = path, .string = "RACE") %>% suppressMessages()
  query_the <- query_src_dir(.src_directory = path, .string = "THE") %>% suppressMessages()
  query_subject <- query_src_dir(.src_directory = path, .string = "CDISC01.200002") %>% suppressMessages()

  expect_true(query_white$DOMAIN == "dm")
  expect_true(query_white$COLUMNS == "RACE")

  expect_true(query_race$COLUMNS[1] == "LBORRES,LBSTRESC,LBSTNRC")
  expect_true(all(query_race$MATCHING == "race"))
  expect_true(query_race$DOMAIN[2] == "mrgda_labels (ae)")

  expect_true(all(c("AETERM", "RACE", "PETEST") %in% query_the$COLUMNS))

  expect_true(nrow(query_subject) == 7)

})

test_that("query_src_dir is not case sensitive", {
  expect_identical(
    query_src_dir(.src_directory = path, .string = "RACE") %>% suppressMessages(),
    query_src_dir(.src_directory = path, .string = "race") %>% suppressMessages()
  )
})
