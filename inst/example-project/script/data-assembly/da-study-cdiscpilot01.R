# Study-level data assembly script for CDISCPILOT01.
#
# This script reads SDTM-like XPT source data for one study and creates a
# NONMEM-style PK analysis dataset. It is intentionally compact, but follows
# the same basic workflow used in a project DA:
#   1. Read source domains with mrgda::read_src_dir().
#   2. Map source domains into subject-level and time-varying derived pieces.
#   3. Combine, derive model-ready timing variables, and assign IDs.
#   4. Validate against a yspec specification and write derived outputs.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(lubridate)
library(assertthat)
library(yspec)
library(mrgda)

here::i_am("inst/example-project/script/data-assembly/da-study-cdiscpilot01.R")

example_project_path <- here("inst", "example-project")
source_path <- file.path(example_project_path, "data", "source", "CDISCPILOT01")
derived_path <- file.path(example_project_path, "data", "derived")

# Source Data -------------------------------------------------------------

src_list <-
  read_src_dir(
    .path = source_path,
    .file_types = "xpt",
    .read_domains = c("dm", "vs", "pc", "ex")
  )

# Specification -----------------------------------------------------------

spec <- ys_load(file.path(derived_path, "pk.yml"))

# Data Storage List -------------------------------------------------------

derived <- list(sl = list(), tv = list())

# Demographics ------------------------------------------------------------

dm_0 <-
  src_list$dm %>%
  filter(ACTARM %in% c("Xanomeline Low Dose", "Xanomeline High Dose"))

derived$sl$studyid <-
  dm_0 %>%
  transmute(
    USUBJID,
    STUDYID
  )

derived$sl$agebl <-
  dm_0 %>%
  transmute(
    USUBJID,
    AGEBL = AGE
  )

derived$sl$sex <-
  dm_0 %>%
  transmute(
    USUBJID,
    SEX = case_when(
      SEX == "F" ~ 0,
      SEX == "M" ~ 1,
      TRUE ~ NA_real_
    )
  )

derived$sl$race <-
  dm_0 %>%
  transmute(
    USUBJID,
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      TRUE ~ 4
    )
  )

assert_that(
  !anyNA(derived$sl$agebl$AGEBL),
  !anyNA(derived$sl$sex$SEX),
  !anyNA(derived$sl$race$RACE)
)

treated_subjects <- dm_0$USUBJID

# Baseline and Time-Varying Weight ---------------------------------------

# VS contains repeated weights over the study. Keep one reusable long
# table here so baseline and time-varying weight are derived consistently.
vs_weight <-
  src_list$vs %>%
  filter(
    USUBJID %in% treated_subjects,
    VSTESTCD == "WEIGHT",
    !is.na(VSSTRESN)
  ) %>%
  mutate(DATETIME = ymd_hms(VSDTC, truncated = 3, tz = "UTC"))

# Prefer the SDTM baseline flag when present. If a subject has no flagged
# baseline weight, the earliest weight is used so the example remains runnable.
derived$sl$wtbl <-
  vs_weight %>%
  arrange(USUBJID, desc(VSBLFL == "Y"), DATETIME) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(USUBJID, WTBL = VSSTRESN)

assert_that(
  nrow(derived$sl$wtbl) == length(treated_subjects),
  !anyNA(derived$sl$wtbl$WTBL)
)

# Weight rows are carried as temporary scaffold events. They are not part of
# the final analysis dataset; they let us fill WT onto nearby dose/PK records.
derived$tv$wt <-
  vs_weight %>%
  transmute(
    USUBJID,
    DATETIME,
    WT = VSSTRESN,
    EVID = -99
  )

# PK Concentrations -------------------------------------------------------

pc_0 <-
  src_list$pc %>%
  filter(
    USUBJID %in% treated_subjects,
    PCTESTCD == "XAN"
  )

derived$tv$pc <-
  pc_0 %>%
  transmute(
    USUBJID,
    DATETIME = ymd_hms(PCDTC, truncated = 3, tz = "UTC"),
    DV = PCSTRESN,
    BLQ = if_else(PCORRES == "<BLQ", 1, 0),
    EVID = 0,
    DVID = 2,
    CMT = 1,
    MDV = if_else(is.na(PCSTRESN), 1, 0),
    AMT = NA_real_,
    RATE = NA_real_,
    DUR = NA_real_
  )

# Every PK record needs a usable collection datetime. BLQ may occur with either
# a missing numeric result or a retained imputed numeric value in this source.
assert_that(
  !anyNA(derived$tv$pc$DATETIME),
  !anyNA(derived$tv$pc$BLQ)
)

# Dosing ------------------------------------------------------------------

ex_0 <-
  src_list$ex %>%
  filter(
    USUBJID %in% treated_subjects,
    EXTRT == "XANOMELINE"
  )

# EX stores treatment intervals rather than one row per administration. Rows
# without an interval end cannot be expanded into daily dose events.
ex_1 <-
  ex_0 %>%
  filter(!(is.na(EXENDTC) | EXENDTC == "")) %>%
  transmute(
    USUBJID,
    AMT = EXDOSE,
    START_DATE = as.Date(substr(EXSTDTC, 1, 10)),
    END_DATE = as.Date(substr(EXENDTC, 1, 10))
  )

assert_that(
  !anyNA(ex_1$START_DATE),
  !anyNA(ex_1$END_DATE),
  all(ex_1$END_DATE >= ex_1$START_DATE)
)

# Expand each treatment interval to one daily dose row. rowwise() lets each
# source interval create its own date sequence; unnest() turns those list
# columns back into ordinary event rows for the model dataset.
derived$tv$dosing <-
  ex_1 %>%
  rowwise() %>%
  mutate(DOSE_DATE = list(seq(START_DATE, END_DATE, by = "day"))) %>%
  ungroup() %>%
  unnest(DOSE_DATE) %>%
  transmute(
    USUBJID,
    DATETIME = as.POSIXct(DOSE_DATE, tz = "UTC"),
    DV = NA_real_,
    BLQ = NA_real_,
    EVID = 1,
    DVID = 1,
    CMT = 1,
    MDV = 1,
    AMT,
    RATE = NA_real_,
    DUR = NA_real_
  )

assert_that(!anyDuplicated(derived$tv$dosing[c("USUBJID", "DATETIME")]))

# Subjects must have at least one expandable dose event before TIME, TAD, and
# DOSE can be anchored. Keep this as a silent filter rather than a warning.
subjects_with_dosing <- unique(derived$tv$dosing$USUBJID)

# Combine -----------------------------------------------------------------

# Merge subject-level pieces into one wide table, then attach them to each
# dose, PK, and temporary weight event.
subject_level <-
  derived$sl %>%
  reduce(full_join, by = "USUBJID") %>%
  filter(USUBJID %in% subjects_with_dosing)

assert_that(!anyDuplicated(subject_level$USUBJID))

pk_0 <-
  bind_rows(derived$tv) %>%
  filter(USUBJID %in% subjects_with_dosing) %>%
  left_join(subject_level, by = "USUBJID") %>%
  arrange(USUBJID, DATETIME, desc(EVID))

assert_that(!anyNA(pk_0$STUDYID))

# Fill time-varying weight onto real dose/PK rows, then drop the temporary
# scaffold rows marked by EVID = -99.
pk_1 <-
  pk_0 %>%
  group_by(USUBJID) %>%
  fill(WT, .direction = "downup") %>%
  ungroup() %>%
  filter(EVID != -99)

assert_that(!anyNA(pk_1$WT))

# DOSE is the current nominal dose for each event. TIME is hours after first
# dose; TAD is hours after the most recent dose.
pk_2 <-
  pk_1 %>%
  mutate(DOSE = if_else(EVID == 1, AMT, NA_real_)) %>%
  group_by(USUBJID) %>%
  fill(DOSE, .direction = "downup") %>%
  mutate(
    FIRST_DOSE_TIME = min(DATETIME[EVID == 1]),
    LAST_DOSE_TIME = replace(DATETIME, EVID != 1, as.POSIXct(NA, tz = "UTC"))
  ) %>%
  fill(LAST_DOSE_TIME, .direction = "down") %>%
  mutate(
    LAST_DOSE_TIME = coalesce(LAST_DOSE_TIME, FIRST_DOSE_TIME),
    TIME = as.numeric(difftime(DATETIME, FIRST_DOSE_TIME, units = "hours")),
    TAD = as.numeric(difftime(DATETIME, LAST_DOSE_TIME, units = "hours")),
    DAY = as.numeric(as.Date(DATETIME) - as.Date(FIRST_DOSE_TIME) + 1)
  ) %>%
  ungroup()

assert_that(
  !anyNA(pk_2$TIME),
  !anyNA(pk_2$TAD),
  !anyNA(pk_2$DAY),
  !anyNA(pk_2$DOSE)
)

# assign_id() gives stable numeric subject IDs. NUM is the within-subject row
# order after all event-level derivations are complete.
pk_3 <-
  pk_2 %>%
  mutate(C = ".") %>%
  assign_id(.subject_col = "USUBJID") %>%
  arrange(ID, DATETIME, desc(EVID)) %>%
  group_by(ID) %>%
  mutate(NUM = row_number()) %>%
  ungroup() %>%
  mutate(
    DATETIME = format(DATETIME, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )

pk_derived <-
  pk_3 %>%
  select(all_of(names(spec)))

# Check and Write ---------------------------------------------------------

ys_check(pk_derived, spec)

write_derived(
  .data = pk_derived,
  .spec = spec,
  .file = file.path(derived_path, "pk.csv"),
  .compare_from_svn = FALSE,
  .execute_diffs = FALSE
)
