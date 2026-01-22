path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path, .file_types = "xpt") %>% suppressMessages()

test_that("query_src_list finds domain and column metadata matches", {
  res <- query_src_list(src_list, .string = "RACE") %>% suppressMessages()

  expect_true(all(c("DOMAIN", "COLUMN", "MATCH_TYPE", "VALUE") %in% names(res)))
  expect_true(any(res$DOMAIN == "dm" & res$COLUMN == "RACE"))
  expect_true(any(res$MATCH_TYPE %in% c("column", "label", "domain")))
})

test_that("query_src_list can search values when requested", {
  res <- query_src_list(src_list, .string = "WHITE") %>% suppressMessages()

  expect_true(any(res$MATCH_TYPE == "value"))
  expect_true(any(!is.na(res$VALUE[res$MATCH_TYPE == "value"])))
})


test_that("query_src_list returns all domain and column combinations with a matching string", {
  query_white <- query_src_list(.src_list = src_list, .string = "WHITE") %>% suppressMessages()
  query_race <- query_src_list(.src_list = src_list, .string = "RACE") %>% suppressMessages()
  query_the <- query_src_list(.src_list = src_list, .string = "THE") %>% suppressMessages()
  query_subject <- query_src_list(.src_list = src_list, .string = "CDISC01.200002") %>% suppressMessages()

  expect_message(
    query_src_list(.src_list = src_list, .string = "No matches zzz"),
    "No matches found for No matches zzz")

  expect_true(query_white$DOMAIN == "dm")
  expect_true(query_white$COLUMN == "RACE")

  expect_true(query_race$COLUMN[1] == "RACE")
  expect_true(query_race$COLUMN[3] == "LBORRES")
  expect_true(query_race$COLUMN[5] == "LBSTRESC")

  expect_true(is.na(query_race$VALUE[1]))
  expect_equal(query_race$VALUE[3], "TRACE")

  expect_true(query_race$DOMAIN[2] == "dm")
  expect_true(query_race$DOMAIN[4] == "lb")

  expect_true(all(c("AETERM", "RACE", "PETEST") %in% query_the$COLUMN))

  expect_true(nrow(query_subject) == 7)

})
