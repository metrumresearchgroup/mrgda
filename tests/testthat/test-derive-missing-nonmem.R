# Tests for missing NONMEM derivation helpers.

test_that("derive_missing_nonmem_tad derives TAD from ID, TIME, and EVID", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 4, 8, 9, 0, 2, 5),
    EVID = c(1, 0, 0, 1, 0, 0, 1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_tad(dat)

  expect_equal(res$TAD, c(0, 1, 4, 0, 1, NA, 0, 3))
})

test_that("derive_missing_nonmem_tad preserves original row order", {
  dat <- data.frame(
    ROW = c("post-second", "first-dose", "post-first", "second-dose"),
    ID = c(1, 1, 1, 1),
    TIME = c(9, 0, 4, 8),
    EVID = c(0, 1, 0, 1)
  )

  res <- mrgda:::derive_missing_nonmem_tad(dat)

  expect_equal(res$ROW, dat$ROW)
  expect_equal(res$TAD, c(1, 0, 4, 0))
})

test_that("derive_missing_nonmem_tad processes dose rows first for TIME ties", {
  dat <- data.frame(
    ROW = c("same-time-observation", "same-time-dose", "post-dose"),
    ID = c(1, 1, 1),
    TIME = c(0, 0, 1),
    EVID = c(0, 1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_tad(dat)

  expect_equal(res$ROW, dat$ROW)
  expect_equal(res$TAD, c(0, 0, 1))
})

test_that("derive_missing_nonmem_tad treats EVID 4 as a dose", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 2, 8, 10),
    EVID = c(1, 0, 4, 0)
  )

  res <- mrgda:::derive_missing_nonmem_tad(dat)

  expect_equal(res$TAD, c(0, 2, 0, 2))
})

test_that("derive_missing_nonmem_tad leaves existing TAD unchanged", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    TAD = c(99, 100)
  )

  res <- mrgda:::derive_missing_nonmem_tad(dat)

  expect_identical(res, dat)
})

test_that("derive_missing_nonmem_tad does nothing when TAD is disabled", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_tad(
    dat,
    .role_overrides = list(tad = FALSE)
  )

  expect_identical(res, dat)
})

test_that("derive_missing_nonmem_tad errors when required roles are missing", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1)
  )

  expect_error(
    mrgda:::derive_missing_nonmem_tad(dat),
    "evid"
  )
})

test_that("derive_missing_nonmem_tad respects role overrides", {
  dat <- data.frame(
    SUBJ = c(1, 1, 1),
    ATIME = c(0, 2, 5),
    EVENT = c(1, 0, 0)
  )

  res <- mrgda:::derive_missing_nonmem_tad(
    dat,
    .role_overrides = list(
      id = "SUBJ",
      time = "ATIME",
      evid = "EVENT",
      tad = "MYTAD"
    )
  )

  expect_equal(res$MYTAD, c(0, 2, 5))
  expect_false("TAD" %in% names(res))
})

test_that("derive_missing_nonmem_occ derives OCC from ID, TIME, and EVID", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1, 1, 2, 2),
    TIME = c(-1, 0, 1, 8, 9, 0, 1),
    EVID = c(0, 1, 0, 1, 0, 1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_equal(res$OCC, c(0, 1, 1, 2, 2, 1, 1))
})

test_that("derive_missing_nonmem_occ preserves original row order", {
  dat <- data.frame(
    ROW = c("post-second", "first-dose", "post-first", "second-dose"),
    ID = c(1, 1, 1, 1),
    TIME = c(9, 0, 4, 8),
    EVID = c(0, 1, 0, 1)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_equal(res$ROW, dat$ROW)
  expect_equal(res$OCC, c(2, 1, 1, 2))
})

test_that("derive_missing_nonmem_occ processes dose rows first for TIME ties", {
  dat <- data.frame(
    ROW = c("same-time-observation", "same-time-dose", "post-dose"),
    ID = c(1, 1, 1),
    TIME = c(0, 0, 1),
    EVID = c(0, 1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_equal(res$ROW, dat$ROW)
  expect_equal(res$OCC, c(1, 1, 1))
})

test_that("derive_missing_nonmem_occ treats EVID 4 as a dose", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 2, 8, 10),
    EVID = c(1, 0, 4, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_equal(res$OCC, c(1, 1, 2, 2))
})

test_that("derive_missing_nonmem_occ leaves existing OCC unchanged", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0),
    OCC = c(99, 100)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_identical(res, dat)
})

test_that("derive_missing_nonmem_occ does nothing when occasion is disabled", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1),
    EVID = c(1, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(
    dat,
    .role_overrides = list(occasion = FALSE)
  )

  expect_identical(res, dat)
})

test_that("derive_missing_nonmem_occ errors when required roles are missing", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 1)
  )

  expect_error(
    mrgda:::derive_missing_nonmem_occ(dat),
    "evid"
  )
})

test_that("derive_missing_nonmem_occ respects role overrides", {
  dat <- data.frame(
    SUBJ = c(1, 1, 1),
    ATIME = c(0, 2, 5),
    EVENT = c(1, 0, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(
    dat,
    .role_overrides = list(
      id = "SUBJ",
      time = "ATIME",
      evid = "EVENT",
      occasion = "MYOCC"
    )
  )

  expect_equal(res$MYOCC, c(1, 1, 1))
  expect_false("OCC" %in% names(res))
})

test_that("derive_missing_nonmem_occ uses BLQ when available", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 8, 9),
    EVID = c(1, 0, 1, 0),
    BLQ = c(NA, 1, NA, 0)
  )

  res <- mrgda:::derive_missing_nonmem_occ(dat)

  expect_equal(res$OCC, c(0, 0, 1, 1))
})
