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

role_map_value <- function(res, role, column) {
  res$role_map[[column]][res$role_map$role == role]
}

# ---- exact-match detection -------------------------------------------------

test_that("resolves default NONMEM column names by exact match", {
  dat <- make_df(c(
    "ID",
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

  expect_equal(res$roles$id, "ID")
  expect_equal(res$roles$subject_label, "SUBJID")
  expect_equal(res$roles$time, "TIME")
  expect_equal(res$roles$tad, "TAD")
  expect_equal(res$roles$dv, "DV")
  expect_equal(res$roles$amt, "AMT")
  expect_equal(res$roles$evid, "EVID")
  expect_equal(res$roles$mdv, "MDV")
  expect_equal(res$roles$rate, "RATE")
  expect_equal(res$roles$dur, "DUR")
  expect_equal(res$roles$blq, "BLQ")
  expect_equal(res$roles$cmt, "CMT")
  expect_equal(res$roles$dvid, "DVID")
  expect_equal(res$roles$dose_group, "DOSE")
  expect_equal(res$roles$day, "DAY")
  expect_equal(res$roles$datetime, "DATETIME")
  expect_equal(res$roles$comment, "C")
  expect_equal(res$roles$occasion, "OCC")

  resolved_roles <- names(res$roles)[!vapply(res$roles, is.null, logical(1))]

  expect_true(all(
    res$role_map$source[res$role_map$role %in% resolved_roles] == "exact name"
  ))
})

test_that("first matching candidate wins when several are present", {
  dat <- make_df(c("USUBJID", "SUBJID", "DATE", "DATETIME", "DROP", "C"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(res$roles$subject_label, "SUBJID")
  expect_equal(res$roles$datetime, "DATETIME")
  expect_equal(res$roles$comment, "C")
})

test_that("alternate candidates resolve when preferred name is absent", {
  dat <- make_df(c("USUBJID", "TAFD", "BLOQ", "DOSEGRP", "DATE", "EXCL"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(res$roles$subject_label, "USUBJID")
  expect_equal(res$roles$tad, "TAFD")
  expect_equal(res$roles$blq, "BLOQ")
  expect_equal(res$roles$dose_group, "DOSEGRP")
  expect_equal(res$roles$datetime, "DATE")
  expect_equal(res$roles$comment, "EXCL")
})

test_that("case-insensitive fallback fires after exact match fails", {
  dat <- make_df(c("id", "usubjid", "time", "dv"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(res$roles$id, "id")
  expect_equal(res$roles$subject_label, "usubjid")
  expect_equal(res$roles$time, "time")
  expect_equal(res$roles$dv, "dv")
  expect_equal(role_map_value(res, "id", "source"), "case-insensitive")
})

test_that("exact match is preferred over case-insensitive match", {
  dat <- make_df(c("id", "ID"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(res$roles$id, "ID")
  expect_equal(role_map_value(res, "id", "source"), "exact name")
})

# ---- unresolved roles ------------------------------------------------------

test_that("unresolved role is NULL with not found and skipped", {
  dat <- make_df(c("ID", "TIME"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_null(res$roles$dv)
  expect_equal(role_map_value(res, "dv", "source"), "not found")
  expect_equal(role_map_value(res, "dv", "status"), "skipped")
  expect_true(is.na(role_map_value(res, "dv", "resolved")))
})

# ---- overrides -------------------------------------------------------------

test_that("override picks the user-specified column", {
  dat <- make_df(c("ID", "TIME", "DV", "CONC"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "CONC")
  )

  expect_equal(res$roles$dv, "CONC")
  expect_equal(role_map_value(res, "dv", "source"), "override")
  expect_equal(role_map_value(res, "dv", "status"), "OK")
})

test_that("override referencing a missing column reports missing column", {
  dat <- make_df(c("ID", "TIME"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "BOGUS")
  )

  expect_null(res$roles$dv)
  expect_equal(role_map_value(res, "dv", "source"), "override")
  expect_equal(role_map_value(res, "dv", "status"), "missing column")
  expect_equal(role_map_value(res, "dv", "resolved"), "BOGUS")
})

test_that("override equal to FALSE disables the role", {
  dat <- make_df(c("ID", "MDV"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(mdv = FALSE)
  )

  expect_null(res$roles$mdv)
  expect_equal(role_map_value(res, "mdv", "source"), "override")
  expect_equal(role_map_value(res, "mdv", "status"), "disabled")
})

test_that("override beats candidate matches", {
  dat <- make_df(c("ID", "DV", "MYCONC"))

  res <- resolve_nonmem_roles(
    .data = dat,
    .role_overrides = list(dv = "MYCONC")
  )

  expect_equal(res$roles$dv, "MYCONC")
  expect_equal(role_map_value(res, "dv", "source"), "override")
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

test_that("unknown override keys are silently ignored", {
  dat <- make_df(c("ID", "TIME"))

  expect_silent(
    res <- resolve_nonmem_roles(
      .data = dat,
      .role_overrides = list(typo_role = "ID")
    )
  )

  expect_equal(res$roles$id, "ID")
})

# ---- role_map / roles list contract ----------------------------------------

test_that("role_map covers every role exactly once in candidate order", {
  dat <- make_df(c("ID"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_equal(nrow(res$role_map), length(mrgda:::default_role_candidates))
  expect_equal(res$role_map$role, names(mrgda:::default_role_candidates))
  expect_equal(names(res$role_map), c("role", "resolved", "source", "status"))
})

test_that("roles list is named for every role", {
  dat <- make_df(c("ID"))
  res <- resolve_nonmem_roles(.data = dat)

  expect_setequal(names(res$roles), names(mrgda:::default_role_candidates))
})

# ---- end-to-end against inst/examples --------------------------------------

test_that("resolves a realistic dataset from inst/examples", {
  csv <- system.file("examples", "pk.csv", package = "mrgda")
  skip_if(!nzchar(csv) || !file.exists(csv), "inst/examples not installed")

  nm <- read_csv_dots(csv)
  res <- resolve_nonmem_roles(.data = nm)

  expect_equal(res$roles$id, "ID")
  expect_equal(res$roles$subject_label, "USUBJID")
  expect_equal(res$roles$time, "TIME")
  expect_equal(res$roles$tad, "TAD")
  expect_equal(res$roles$dv, "DV")
  expect_equal(res$roles$amt, "AMT")
  expect_equal(res$roles$evid, "EVID")
  expect_equal(res$roles$mdv, "MDV")
  expect_equal(res$roles$cmt, "CMT")
  expect_equal(res$roles$blq, "BLQ")
  expect_equal(res$roles$dose_group, "DOSE")
  expect_equal(res$roles$comment, "C")
})
