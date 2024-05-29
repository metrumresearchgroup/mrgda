test_that("read_nm_csv works as expected", {
  file1 <- system.file("derived/pk.csv", package = "mrgda")

  df <- read_nm_csv(file1) %>% suppressMessages()

  expect_true(inherits(df, "data.frame"))
  expect_true(is.na(unique(df$C)))
})
