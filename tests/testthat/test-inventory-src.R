test_that("inventory_src summarizes domains and variables", {
  dm <- tibble::tibble(
    USUBJID = c("01", "02"),
    AGE = c(30, 40)
  )
  attr(dm$AGE, "label") <- "Age"

  pc <- tibble::tibble(
    USUBJID = c("01", "01", "02"),
    PCSEQ = c(1, 2, 1),
    PCDTC = c("2024-01-01T08:00", "2024-01-01T09:00", "2024-01-02T08:00"),
    PCSTRESN = c(1.2, NA, 3.4)
  )

  inventory <- inventory_src(list(dm = dm, pc = pc))

  expect_s3_class(inventory, "mrgda_src_inventory")
  expect_named(inventory, c("domains", "variables"))
  expect_equal(inventory$domains$N_ROWS, c(2, 3))
  expect_equal(inventory$domains$N_SUBJECTS, c(2, 2))
  expect_equal(inventory$domains$SEQUENCE_VARIABLE, c(NA, "PCSEQ"))
  expect_equal(inventory$domains$DATETIME_VARIABLES, c(NA, "PCDTC"))
  expect_equal(inventory$domains$RESULT_VARIABLES, c(NA, "PCSTRESN"))
  expect_true(inventory$domains$KEY_UNIQUE[inventory$domains$DOMAIN == "pc"])

  age <- inventory$variables[
    inventory$variables$DOMAIN == "dm" & inventory$variables$VARIABLE == "AGE",
  ]
  expect_equal(age$LABEL, "Age")
  expect_equal(age$CLASS, "numeric")
  expect_equal(age$N_MISSING, 0)
  expect_equal(age$N_DISTINCT, 2)
})

test_that("inventory_src uses canonical domain order", {
  inventory <- inventory_src(list(
    pc = tibble::tibble(USUBJID = "01", PCSEQ = 1),
    dm = tibble::tibble(USUBJID = "01")
  ))

  expect_equal(inventory$domains$DOMAIN, c("dm", "pc"))
})

test_that("inventory_src uses labels and excludes mrgda metadata", {
  dm <- tibble::tibble(USUBJID = "01", AGE = 30)
  labels <- tibble::tibble(
    DOMAIN = "dm",
    COLUMN_NAME = "AGE",
    COLUMN_LABEL = "Age from metadata"
  )
  src <- list(
    dm = dm,
    mrgda_labels = labels,
    mrgda_src_meta = list(type = "xpt")
  )

  inventory <- inventory_src(src)

  expect_equal(inventory$domains$DOMAIN, "dm")
  expect_false(any(startsWith(inventory$domains$DOMAIN, "mrgda_")))
  expect_equal(
    inventory$variables$LABEL[inventory$variables$VARIABLE == "AGE"],
    "Age from metadata"
  )
})

test_that("inventory_src excludes the source-correction audit", {
  src <- list(
    dm = tibble::tibble(USUBJID = "01", AGE = 30),
    corrections = tibble::tibble(
      domain = "dm",
      USUBJID = "01",
      variable = "AGE"
    )
  )

  inventory <- inventory_src(src)

  expect_equal(inventory$domains$DOMAIN, "dm")
  expect_false("corrections" %in% inventory$variables$DOMAIN)
})

test_that("inventory_src reports inferred-key problems", {
  pc <- tibble::tibble(
    USUBJID = c("01", "01", NA_character_),
    PCSEQ = c(1, 1, NA_real_),
    PCDTC = c("2024-01-01", "2024-01-01", "2024-01-02")
  )

  inventory <- inventory_src(list(pc = pc))

  expect_equal(inventory$domains$N_MISSING_SUBJECT, 1)
  expect_equal(inventory$domains$N_MISSING_SEQUENCE, 1)
  expect_equal(inventory$domains$N_EXCESS_KEY_ROWS, 1)
  expect_equal(inventory$domains$N_EXACT_DUPLICATE_ROWS, 1)
  expect_false(inventory$domains$KEY_UNIQUE)
})

test_that("inventory_src validates its inputs", {
  expect_error(inventory_src(data.frame(x = 1)), "named list")
  expect_error(inventory_src(list(metadata = "value")), "source data frames")
  expect_error(
    inventory_src(list(dm = data.frame(x = 1)), .subject_col = character()),
    "single non-empty string"
  )
})

test_that("inventory_src prints a compact summary", {
  inventory <- inventory_src(list(dm = tibble::tibble(USUBJID = "01")))

  expect_message(print(inventory), "Source inventory")
  expect_message(print(inventory), "1 domain")
})
