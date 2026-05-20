test_that("derived_to_here summarizes derived data frames", {
  derived <- list(
    sl = list(
      adsl = data.frame(
        USUBJID = c("01", "02"),
        ARM = c("A", "B"),
        stringsAsFactors = FALSE
      ),
      adae = data.frame(
        USUBJID = "01",
        AEDECOD = "HEADACHE",
        AESER = "N",
        stringsAsFactors = FALSE
      )
    ),
    tv = list(
      qc_adsl = data.frame(
        USUBJID = c("01", "02"),
        FLAG = c("Y", "N"),
        TIME = c(0, 2),
        stringsAsFactors = FALSE
      )
    )
  )

  res <- derived_to_here(derived)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("df_name", "type", "columns"))
  expect_equal(res$df_name, c("adsl", "adae", "qc_adsl"))
  expect_equal(res$type, c("sl", "sl", "tv"))
  expect_equal(
    res$columns,
    c("USUBJID, ARM", "USUBJID, AEDECOD, AESER", "USUBJID, FLAG, TIME")
  )
})

test_that("derived_to_here records empty column summaries", {
  derived <- list(
    sl = list(
      adsl = data.frame(row.names = 1:2)
    )
  )

  res <- derived_to_here(derived)

  expect_equal(res$df_name, "adsl")
  expect_equal(res$type, "sl")
  expect_equal(res$columns, "")
})
