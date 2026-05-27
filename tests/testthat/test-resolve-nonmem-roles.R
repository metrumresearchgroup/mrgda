# Tests for resolve_nonmem_roles()
#
# Helpers construct minimal fixtures so each behavior of the resolver is
# exercised in isolation, without depending on external CSV files. A final
# block round-trips on inst/examples to guard against regressions on a
# realistic dataset.

make_df <- function(cols) {
  df <- data.frame(
    matrix(numeric(0), nrow = 0, ncol = length(cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- cols
  df
}

role_result_value <- function(res, role, column) {
  res[[column]][res$role == role]
}

expect_role_result <- function(
  res,
  role,
  column,
  source = "default name",
  status = "OK"
) {
  expect_equal(role_result_value(res, role, "column"), column)
  expect_equal(role_result_value(res, role, "source"), source)
  expect_equal(role_result_value(res, role, "status"), status)
}

# ---- default column detection ----------------------------------------------

test_that("resolves default NONMEM column names", {
  dat <- make_df(c(
    "STUDYID",
    "ID",
    "USUBJID",
    "SUBJID",
    "TIME",
    "TAD",
    "DV",
    "AMT",
    "EVID",
    "MDV",
    "RATE",
    "DUR",
    "BLQ",
    "CMT",
    "DVID",
    "DOSE",
    "DAY",
    "DATETIME",
    "C",
    "OCC"
  ))

  res <- resolve_nonmem_roles(.data = dat)

  expect_role_result(res, "study_id", "STUDYID")
  expect_role_result(res, "id", "ID")
  expect_role_result(res, "subject_label", "USUBJID")
  expect_role_result(res, "time", "TIME")
  expect_role_result(res, "tad", "TAD")
  expect_role_result(res, "dv", "DV")
  expect_role_result(res, "amt", "AMT")
  expect_role_result(res, "evid", "EVID")
  expect_role_result(res, "mdv", "MDV")
  expect_role_result(res, "rate", "RATE")
  expect_role_result(res, "dur", "DUR")
  expect_role_result(res, "blq", "BLQ")
  expect_role_result(res, "cmt", "CMT")
  expect_role_result(res, "dvid", "DVID")
  expect_role_result(res, "dose_group", "DOSE")
  expect_role_result(res, "day", "DAY")
  expect_role_result(res, "datetime", "DATETIME")
  expect_role_result(res, "comment", "C")
  expect_role_result(res, "occasion", "OCC")
})

test_that("first default column name wins when several are present", {
  dat <- make_df(c(
    "STUDY",
    "STUDYID",
    "SUBJID",
    "USUBJID",
    "TAFD",
    "TIME",
    "TSD",
    "TAD",
    "DATE",
    "DATETIME",
    "DROP",
    "C"
  ))

  res <- resolve_nonmem_roles(.data = dat)

  expect_role_result(res, "study_id", "STUDYID")
  expect_role_result(res, "subject_label", "USUBJID")
  expect_role_result(res, "time", "TIME")
  expect_role_result(res, "tad", "TAD")
  expect_role_result(res, "datetime", "DATETIME")
  expect_role_result(res, "comment", "C")
})

test_that("alternate default column names resolve when preferred names are absent", {
  dat <- make_df(c(
    "STUDY",
    "SUBJID",
    "TAFD",
    "TSD",
    "BLOQ",
    "DOSEGRP",
    "DATE",
    "EXCL"
  ))

  res <- resolve_nonmem_roles(.data = dat)

  expect_role_result(res, "study_id", "STUDY")
  expect_role_result(res, "subject_label", "SUBJID")
  expect_role_result(res, "time", "TAFD")
  expect_role_result(res, "tad", "TSD")
  expect_role_result(res, "blq", "BLOQ")
  expect_role_result(res, "dose_group", "DOSEGRP")
  expect_role_result(res, "datetime", "DATE")
  expect_role_result(res, "comment", "EXCL")
})

test_that("later alternate default column names resolve", {
  dat <- make_df(c("BQL", "IGNORE"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_role_result(res, "blq", "BQL")
  expect_role_result(res, "comment", "IGNORE")
})

test_that("last alternate default column name resolves", {
  dat <- make_df(c("DROP"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_role_result(res, "comment", "DROP")
})

# ---- unresolved roles ------------------------------------------------------

test_that("unresolved role has missing column with not found and skipped", {
  dat <- make_df(c("ID", "TIME"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_true(is.na(role_result_value(res, "dv", "column")))
  expect_equal(role_result_value(res, "dv", "source"), "not found")
  expect_equal(role_result_value(res, "dv", "status"), "skipped")
})

test_that("unresolved roles remain present in the output", {
  dat <- make_df(c("ID"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_true("study_id" %in% res$role)
  expect_true("time" %in% res$role)
  expect_true("dv" %in% res$role)

  expect_true(is.na(role_result_value(res, "study_id", "column")))
  expect_true(is.na(role_result_value(res, "time", "column")))
  expect_true(is.na(role_result_value(res, "dv", "column")))
})

# ---- overrides -------------------------------------------------------------

test_that("override picks the user-specified column", {
  dat <- make_df(c("ID", "TIME", "DV", "CONC"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "CONC")
  )

  expect_role_result(
    res = res,
    role = "dv",
    column = "CONC",
    source = "override",
    status = "OK"
  )
})

test_that("override referencing a missing column reports missing column", {
  dat <- make_df(c("ID", "TIME"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "BOGUS")
  )

  expect_true(is.na(role_result_value(res, "dv", "column")))
  expect_equal(role_result_value(res, "dv", "source"), "override")
  expect_equal(role_result_value(res, "dv", "status"), "missing column")
})

test_that("override equal to FALSE disables the role", {
  dat <- make_df(c("ID", "MDV"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(mdv = FALSE)
  )

  expect_true(is.na(role_result_value(res, "mdv", "column")))
  expect_equal(role_result_value(res, "mdv", "source"), "override")
  expect_equal(role_result_value(res, "mdv", "status"), "disabled")
})

test_that("override beats default column name", {
  dat <- make_df(c("ID", "DV", "MYCONC"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "MYCONC")
  )

  expect_role_result(
    res = res,
    role = "dv",
    column = "MYCONC",
    source = "override",
    status = "OK"
  )
})

test_that("override must be one column name", {
  dat <- make_df(c("ID", "DV", "CONC"))

  expect_error(
    resolve_nonmem_roles(
      .data = dat,
      .role_overrides = list(dv = c("DV", "CONC"))
    ),
    "must be one column name"
  )
})

test_that("NULL role_overrides uses default column names", {
  dat <- make_df(c("ID", "DV"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = NULL
  )

  expect_role_result(res, "id", "ID")
  expect_role_result(res, "dv", "DV")
})

test_that("empty override value falls back to default column names", {
  dat <- make_df(c("ID", "DV"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = character())
  )

  expect_role_result(res, "dv", "DV")
})

test_that("unknown override keys are silently ignored", {
  dat <- make_df(c("ID", "TIME"))

  expect_silent(
    res <- resolve_nonmem_roles(
      .data = dat,
      .role_overrides = list(typo_role = "BOGUS")
    )
  )

  expect_role_result(res, "id", "ID")
  expect_role_result(res, "time", "TIME")
})

# ---- input checks ----------------------------------------------------------

test_that("data must be a data frame", {
  expect_error(
    resolve_nonmem_roles(.data = list(ID = 1)),
    "must be a data frame"
  )
})

test_that("role_overrides must be a list", {
  dat <- make_df(c("ID", "DV"))

  expect_error(
    resolve_nonmem_roles(
      .data = dat,
      .role_overrides = "DV"
    ),
    "must be a list"
  )
})

# ---- output contract -------------------------------------------------------

test_that("output covers every role exactly once in default order", {
  dat <- make_df(c("ID"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(
    nrow(res),
    length(mrgda:::default_nonmem_column_names_by_role)
  )

  expect_equal(
    res$role,
    names(mrgda:::default_nonmem_column_names_by_role)
  )
})

test_that("output has expected columns", {
  dat <- make_df(c("ID"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(
    names(res),
    c("role", "column", "source", "status")
  )
})

test_that("output has one row per role and no duplicate roles", {
  dat <- make_df(c("ID"))

  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(nrow(res), length(unique(res$role)))
})

# ---- end-to-end against inst/examples --------------------------------------

test_that("resolves a realistic dataset from inst/examples", {
  csv <- system.file("examples", "pk.csv", package = "mrgda")
  skip_if(!nzchar(csv) || !file.exists(csv), "inst/examples not installed")

  nm <- read_csv_dots(csv)
  res <- resolve_nonmem_roles(.data = nm)

  expect_role_result(res, "id", "ID")
  expect_role_result(res, "subject_label", "USUBJID")
  expect_role_result(res, "time", "TIME")
  expect_role_result(res, "tad", "TAD")
  expect_role_result(res, "dv", "DV")
  expect_role_result(res, "amt", "AMT")
  expect_role_result(res, "evid", "EVID")
  expect_role_result(res, "mdv", "MDV")
  expect_role_result(res, "cmt", "CMT")
  expect_role_result(res, "blq", "BLQ")
  expect_role_result(res, "dose_group", "DOSE")
  expect_role_result(res, "comment", "C")
})
