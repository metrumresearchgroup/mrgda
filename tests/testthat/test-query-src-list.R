path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path) %>% suppressMessages()

test_that("query_src_list returns all domain and column combinations with a matching string", {
  query_white <- query_src_list(.src_list = src_list, .string = "WHITE") %>% suppressMessages()
  query_race <- query_src_list(.src_list = src_list, .string = "RACE") %>% suppressMessages()
  query_the <- query_src_list(.src_list = src_list, .string = "THE") %>% suppressMessages()
  query_subject <- query_src_list(.src_list = src_list, .string = "CDISC01.200002") %>% suppressMessages()

  expect_message(
    query_src_list(.src_list = src_list, .string = "No matches zzz"),
    "No matches found for No matches zzz")

  expect_true(query_white$DOMAIN == "dm")
  expect_true(query_white$COLUMNS == "RACE")

  expect_true(query_race$COLUMNS[1] == "LBORRES,LBSTRESC,LBSTNRC")
  expect_true(all(query_race$MATCHING == "RACE"))
  expect_true(query_race$DOMAIN[2] == "mrgda_labels")

  expect_true(all(c("AETERM", "RACE", "PETEST") %in% query_the$COLUMNS))

  expect_true(nrow(query_subject) == 7)

})

test_that("query_src_list works with additional strings", {
  query_qtcf <- query_src_list(.src_list = src_list, .string = "QTcF") %>% suppressMessages()
  expect_true(query_qtcf$DOMAIN == "eg")
  expect_true(query_qtcf$COLUMNS == "EGTESTCD,EGTEST")

  query_alzheimers <-query_src_list(.src_list = src_list, .string = "ALZHEIMER'S") %>% suppressMessages()
  expect_true(query_alzheimers$DOMAIN == "mh")
  expect_true(query_alzheimers$COLUMNS == "MHTERM")

  query_cardio <- query_src_list(.src_list = src_list, .string = "Cardio") %>% suppressMessages()
  expect_true(query_cardio$DOMAIN == "pe")
  expect_true(query_cardio$COLUMNS == "PETEST")

  query_cardo <-query_src_list(.src_list = src_list, .string = "Cardo") %>% suppressMessages()
  expect_null(query_cardo)

})
