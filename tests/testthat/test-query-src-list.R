path <- system.file("example-sdtm", package = "mrgda")
src_list <- read_src_dir(.path = path, .file_types = "xpt") %>% suppressMessages()

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

  expect_true(query_race$COLUMNS[2] == "LBORRES,LBSTRESC,LBSTNRC")
  expect_true(all(query_race$MATCHING == "RACE"))
  expect_true(query_race$DOMAIN[3] == "mrgda_labels")

  expect_true(all(c("AETERM", "RACE", "PETEST") %in% query_the$COLUMNS))

  expect_true(nrow(query_subject) == 7)

})

test_that("query_src_list matches column names, labels, and values", {
  df <- data.frame(RACE_CODE = c("White", "Black"), stringsAsFactors = FALSE)
  attr(df$RACE_CODE, "label") <- "Race Description"
  src_list_local <- list(dm = df)

  query_name <- query_src_list(.src_list = src_list_local, .string = "RACE") %>% suppressMessages()
  query_label <- query_src_list(.src_list = src_list_local, .string = "Description") %>% suppressMessages()
  query_value <- query_src_list(.src_list = src_list_local, .string = "White") %>% suppressMessages()

  expect_true(query_name$COLUMNS == "RACE_CODE")
  expect_true(query_label$COLUMNS == "RACE_CODE")
  expect_true(query_value$COLUMNS == "RACE_CODE")
})
