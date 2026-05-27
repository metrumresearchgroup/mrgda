# Tests for resolve_nonmem_roles()
#
# Helpers `make_df()` and `make_spec()` construct minimal fixtures so each
# behavior of the resolver is exercised in isolation, without depending on
# external CSV/yml files. A final block round-trips on inst/examples to
# guard against regressions on a realistic spec.

make_df <- function(cols) {
  df <- data.frame(
    matrix(numeric(0), nrow = 0, ncol = length(cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- cols
  df
}

make_spec <- function(flags = NULL) {
  spec <- list()
  attr(spec, "meta") <- list(flags = flags)
  spec
}

role_map_value <- function(res, role, column) {
  res$role_map[[column]][res$role_map$role == role]
}

# ---- exact-match detection -------------------------------------------------

test_that("resolves canonical NONMEM column names by exact match", {
  dat <- make_df(c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "BLQ", "CMT"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())

  expect_equal(res$roles$id, "ID")
  expect_equal(res$roles$time, "TIME")
  expect_equal(res$roles$dv, "DV")
  expect_equal(res$roles$amt, "AMT")
  expect_equal(res$roles$evid, "EVID")
  expect_equal(res$roles$mdv, "MDV")
  expect_equal(res$roles$blq, "BLQ")
  expect_equal(res$roles$cmt, "CMT")
  exact_roles <- c("id", "time", "dv", "amt", "evid", "mdv")
  expect_true(all(
    res$role_map$source[res$role_map$role %in% exact_roles] == "exact name"
  ))
})

test_that("first matching candidate wins when several are present", {
  # ID and SUBJID both qualify for `id`; ID comes first in the candidate list.
  dat <- make_df(c("ID", "SUBJID"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_equal(res$roles$id, "ID")
})

test_that("alternate candidates resolve when canonical name is absent", {
  dat <- make_df(c("SUBJID", "TIMEA", "CONC", "DAMT"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_equal(res$roles$id, "SUBJID")
  expect_equal(res$roles$time, "TIMEA")
  expect_equal(res$roles$dv, "CONC")
  expect_equal(res$roles$amt, "DAMT")
})

test_that("case-insensitive fallback fires after exact match fails", {
  dat <- make_df(c("id", "time", "dv"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_equal(res$roles$id, "id")
  expect_equal(res$roles$time, "time")
  expect_equal(res$roles$dv, "dv")
  expect_equal(role_map_value(res, "id", "source"), "case-insensitive")
})

test_that("exact match preferred over case-insensitive when both available", {
  dat <- make_df(c("id", "ID"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_equal(res$roles$id, "ID")
  expect_equal(role_map_value(res, "id", "source"), "exact name")
})

# ---- unresolved roles ------------------------------------------------------

test_that("unresolved scalar role is NULL with 'not found' / 'skipped'", {
  dat <- make_df(c("ID", "TIME"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_null(res$roles$dv)
  expect_equal(role_map_value(res, "dv", "source"), "not found")
  expect_equal(role_map_value(res, "dv", "status"), "skipped")
  expect_true(is.na(role_map_value(res, "dv", "resolved")))
})

test_that("unresolved multi-column role is character() with 'not found'", {
  dat <- make_df(c("ID"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_identical(res$roles$bl_cat_cov, character())
  expect_equal(role_map_value(res, "bl_cat_cov", "status"), "skipped")
})

# ---- scalar overrides ------------------------------------------------------

test_that("scalar override picks the user-specified column", {
  dat <- make_df(c("ID", "TIME", "DV", "CONC"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = make_spec(),
    .role_overrides = list(dv = "CONC")
  )
  expect_equal(res$roles$dv, "CONC")
  expect_equal(role_map_value(res, "dv", "source"), "override")
  expect_equal(role_map_value(res, "dv", "status"), "OK")
})

test_that("scalar override referencing a missing column reports missing column", {
  dat <- make_df(c("ID", "TIME"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = make_spec(),
    .role_overrides = list(dv = "BOGUS")
  )
  expect_null(res$roles$dv)
  expect_equal(role_map_value(res, "dv", "source"), "override")
  expect_equal(role_map_value(res, "dv", "status"), "missing column")
  expect_equal(role_map_value(res, "dv", "resolved"), "BOGUS")
})

test_that("scalar override = FALSE disables the role", {
  dat <- make_df(c("ID", "MDV"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = make_spec(),
    .role_overrides = list(mdv = FALSE)
  )
  expect_null(res$roles$mdv)
  expect_equal(role_map_value(res, "mdv", "source"), "override")
  expect_equal(role_map_value(res, "mdv", "status"), "disabled")
})

test_that("scalar override beats both candidate matches", {
  # `dv` would normally resolve to DV by exact-name; the override should
  # force the choice of an unconventional column.
  dat <- make_df(c("ID", "DV", "MYCONC"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = make_spec(),
    .role_overrides = list(dv = "MYCONC")
  )
  expect_equal(res$roles$dv, "MYCONC")
})

# ---- multi-column roles & flags --------------------------------------------

test_that("multi-column role resolves from yspec flags", {
  dat <- make_df(c("ID", "SEX", "RACE", "AGE"))
  spec <- make_spec(flags = list(bl_cat_cov = c("SEX", "RACE")))
  res <- resolve_nonmem_roles(.data = dat, .spec = spec)
  expect_equal(res$roles$bl_cat_cov, c("SEX", "RACE"))
  expect_equal(role_map_value(res, "bl_cat_cov", "source"), "yspec flags")
  expect_equal(role_map_value(res, "bl_cat_cov", "status"), "OK")
})

test_that("multi-column flag with a missing column annotates status", {
  dat <- make_df(c("ID", "SEX"))
  spec <- make_spec(flags = list(bl_cat_cov = c("SEX", "RACE")))
  res <- resolve_nonmem_roles(.data = dat, .spec = spec)
  expect_equal(res$roles$bl_cat_cov, "SEX")
  expect_match(role_map_value(res, "bl_cat_cov", "status"), "missing: RACE")
})

test_that("multi-column override takes precedence over yspec flags", {
  dat <- make_df(c("ID", "WT", "AGE", "BMI"))
  spec <- make_spec(flags = list(bl_cont_cov = "AGE"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = spec,
    .role_overrides = list(bl_cont_cov = c("WT", "BMI"))
  )
  expect_equal(res$roles$bl_cont_cov, c("WT", "BMI"))
  expect_equal(role_map_value(res, "bl_cont_cov", "source"), "override")
})

test_that("multi-column override = FALSE disables a role even when flagged", {
  dat <- make_df(c("ID", "SEX"))
  spec <- make_spec(flags = list(bl_cat_cov = c("SEX", "RACE")))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = spec,
    .role_overrides = list(bl_cat_cov = FALSE)
  )
  expect_identical(res$roles$bl_cat_cov, character())
  expect_equal(role_map_value(res, "bl_cat_cov", "status"), "disabled")
})

test_that("multi-column character() override clears the role", {
  dat <- make_df(c("ID", "SEX", "RACE"))
  spec <- make_spec(flags = list(bl_cat_cov = c("SEX", "RACE")))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = spec,
    .role_overrides = list(bl_cat_cov = character())
  )
  expect_identical(res$roles$bl_cat_cov, character())
  expect_equal(role_map_value(res, "bl_cat_cov", "source"), "override")
})

test_that("multi-column override status flags columns not present in data", {
  dat <- make_df(c("ID", "WT"))
  res <- resolve_nonmem_roles(
    .data = dat,
    .spec = make_spec(),
    .role_overrides = list(bl_cont_cov = c("WT", "AGE", "EGFR"))
  )
  expect_equal(res$roles$bl_cont_cov, "WT")
  expect_match(
    role_map_value(res, "bl_cont_cov", "status"),
    "missing: AGE, EGFR"
  )
})

# ---- role_map / roles list contract ----------------------------------------

test_that("role_map covers every role exactly once in canonical order", {
  dat <- make_df(c("ID"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_equal(nrow(res$role_map), 23)
  expect_equal(
    res$role_map$role,
    c(
      names(mrgda:::default_role_candidates),
      "bl_cat_cov",
      "bl_cont_cov",
      "tv_cat_cov",
      "tv_cont_cov",
      "occ_cov"
    )
  )
  expect_equal(names(res$role_map), c("role", "resolved", "source", "status"))
})

test_that("roles list is named for every role", {
  dat <- make_df(c("ID"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec())
  expect_setequal(
    names(res$roles),
    c(
      names(mrgda:::default_role_candidates),
      "bl_cat_cov",
      "bl_cont_cov",
      "tv_cat_cov",
      "tv_cont_cov",
      "occ_cov"
    )
  )
})

# ---- spec resilience -------------------------------------------------------

test_that("spec without a meta attribute still resolves scalar roles", {
  dat <- make_df(c("ID", "TIME", "DV"))
  res <- resolve_nonmem_roles(.data = dat, .spec = list())
  expect_equal(res$roles$id, "ID")
  expect_identical(res$roles$bl_cat_cov, character())
})

test_that("spec with empty flags resolves nothing for multi-column roles", {
  dat <- make_df(c("ID", "SEX", "RACE"))
  res <- resolve_nonmem_roles(.data = dat, .spec = make_spec(flags = list()))
  expect_identical(res$roles$bl_cat_cov, character())
  expect_equal(role_map_value(res, "bl_cat_cov", "source"), "not found")
})

test_that("unknown override keys are silently ignored", {
  dat <- make_df(c("ID", "TIME"))
  expect_silent(
    res <- resolve_nonmem_roles(
      .data = dat,
      .spec = make_spec(),
      .role_overrides = list(typo_role = "ID")
    )
  )
  expect_equal(res$roles$id, "ID")
})

# ---- end-to-end against inst/examples --------------------------------------

test_that("resolves a realistic spec from inst/examples", {
  skip_if_not_installed("yspec")
  csv <- system.file("examples", "pk.csv", package = "mrgda")
  yml <- system.file("examples", "pk.yml", package = "mrgda")
  skip_if(!nzchar(csv) || !file.exists(csv), "inst/examples not installed")

  spec <- yspec::load_spec(yml)
  nm <- read_csv_dots(csv)
  res <- resolve_nonmem_roles(.data = nm, .spec = spec)

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
