
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrgda <a href='https:/metrumresearchgroup.github.io/mrgda'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml)
<!-- badges: end -->

```r
###############################################################################
# Purpose:
#   1. Assemble an analysis-ready PK dataset from SDTM-like source domains.
#   2. Produce a single CSV file suitable for NONMEM, with additional metadata 
#      checks via a yspec specification.
###############################################################################

# Load libraries for:
#   - Data manipulation and piping (`tidyverse`)
#   - File path management (`here`)
#   - Assertion checks (`assertthat`)
#   - yspec specifications (`yspec`)
#   - Reading source data from a standardized directory (`mrgda`)
library(tidyverse)
library(here)
library(assertthat)
library(yspec)
library(mrgda)

# Prepare a list to store data pieces
# 'sl' = subject-level data
# 'tv' = time-varying data
out <- list(sl = list(), tv = list())

# -----------------------------------------------------------------------------
# 1. Spec
#    We load a specification file for our final PK dataset schema.
#    This spec tells us which columns we expect, their labels, units, and so forth.
# -----------------------------------------------------------------------------
pk_spec <- ys_load(here("data/derived/pk.yaml"))

# -----------------------------------------------------------------------------
# 2. Source data
#    Read in all SDTM-like data from the study (here study "100"), 
#    storing them in a named list. For instance:
#    src_100$dm  -> Demographics domain
#    src_100$vs  -> Vitals domain
#    src_100$pc  -> Concentrations domain
#    src_100$ex  -> Exposure (dosing) domain
# -----------------------------------------------------------------------------
src_100 <- read_src_dir(here::here("data", "source", "100"))

# -----------------------------------------------------------------------------
# 3. Demographics (DM domain)
#    We'll transform certain demographic variables into numeric codes 
#    and rename as needed for the final dataset.
# -----------------------------------------------------------------------------

# Quick look at some variables in DM to see their possible values
src_100$dm %>% 
  pivot_longer(c(ACTARM, SEX, RACE, ETHNIC)) %>% 
  count(name, value)

# Create a subject-level demographics dataset
dm_1 <-
  src_100$dm %>%
  transmute(
    # Keep subject ID and study ID
    USUBJID,
    STUDYID,
    
    # Convert SEX from 'F'/'M' to numeric (1 or 2); use -99 for unexpected values
    SEX = case_when(
      SEX == "F" ~ 1,
      SEX == "M" ~ 2, 
      TRUE ~ -99
    ),
    
    # Convert RACE to numeric categories; default to -99 if not recognized
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 3,
      RACE == "OTHER" ~ 6,
      TRUE ~ -99
    ),
    
    # Baseline age
    BLAGE = AGE
  )

# Store in our subject-level list
out$sl$dm <- dm_1

# Check we didn't code any rows as -99 in the demographics data 
assert_that(!(any(out$sl$dm == -99, na.rm = TRUE)))

# -----------------------------------------------------------------------------
# 4. Vitals (VS domain)
#    We'll look for baseline weight and height, then compute BMI.
# -----------------------------------------------------------------------------

# Quick summary of test codes, baseline flags, and units
src_100$vs %>% 
  count(VSTESTCD, VSBLFL, VSSTRESU)

# Filter the Vitals data to only include subjects in our DM dataset
vs_0 <-
  src_100$vs %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

# We'll only look for these two test codes in VSTESTCD
vs_tests <- c("WEIGHT", "HEIGHT")

# Filter and reshape the data to capture baseline weight and height
vs_1 <-
  vs_0 %>%
  filter(
    VSBLFL == "Y",              # baseline records only
    VSTESTCD %in% vs_tests      # only weight & height
  ) %>%
  select(
    USUBJID,
    VSTESTCD,
    VSSTRESN
  ) %>%
  # Pivot so that WEIGHT and HEIGHT each become their own column
  pivot_wider(
    names_from = VSTESTCD, 
    values_from = VSSTRESN
  ) %>%
  # Create new columns: BLWT (baseline weight) and baseline BMI
  transmute(
    USUBJID,
    BLWT = WEIGHT, 
    BLBMI = WEIGHT / ((HEIGHT / 100) ^ 2)
  )

# Store in subject-level list
out$sl$vs <- vs_1

# Ensure no subject is duplicated and no missing values
assert_that(!any(duplicated(out$sl$vs$USUBJID)))
assert_that(!(anyNA(out$sl$vs)))

# -----------------------------------------------------------------------------
# 5. PK Concentrations (PC domain)
#    We'll extract drug concentrations, mark BLQ, and set EVID/MDV appropriately.
# -----------------------------------------------------------------------------

# Quick look at PC domain test names and statuses
src_100$pc %>% 
  count(PCTEST, PCSTAT)

# Look at rows where the numeric result (PCSTRESN) is missing
src_100$pc %>% 
  filter(is.na(PCSTRESN)) %>% 
  count(PCTEST, PCORRES, PCSTAT)

# Filter the PC data to keep only:
#   - subjects in DM
#   - test name "DRUG-X"
#   - records that are not "NOT DONE"
pc_0 <-
  src_100$pc %>%
  filter(
    USUBJID %in% out$sl$dm$USUBJID,
    PCTEST == "DRUG-X",
    PCSTAT != "NOT DONE"
  )

# Transmute to create final PK columns
pc_1 <-
  pc_0 %>%
  transmute(
    USUBJID,
    DV = PCSTRESN,  # observed concentration
    # BLQ is 1 if the original string was "<LLOQ", else 0
    BLQ = case_when(PCORRES == "<LLOQ" ~ 1, TRUE ~ 0),
    # EVID=2 for BLQ records (common NONMEM coding), else 0
    EVID = case_when(BLQ == 1 ~ 2, TRUE ~ 0),
    # MDV=0 for BLQ (we want to keep the observation), else 1
    MDV = case_when(BLQ == 1 ~ 0, TRUE ~ 1),
    # Convert string date-time to R date-time object
    DATETIME = ymd_hms(PCDTC),
    # Lower limit of quantification
    LLOQ = PCLLOQ
  )

# Quick check on how many rows are missing DV vs. BLQ-coded
pc_1 %>% 
  count(is.na(DV), BLQ)

# Store in time-varying list
out$tv$pc <- pc_1

# Verify uniqueness of (USUBJID, DATETIME) pairs and no missing DATETIME
assert_that(
  !any(duplicated(out$tv$pc[, c("USUBJID", "DATETIME")])),
  !anyNA(out$tv$pc$DATETIME)
)

# -----------------------------------------------------------------------------
# 6. Dosing (EX domain)
#    We'll set up dosing records for NONMEM, typically EVID=1 and MDV=1.
# -----------------------------------------------------------------------------

# Quick look at EX domain fields
src_100$ex %>% 
  count(EXTRT, EXDOSFRM, EXDOSFRQ, EXDOSU, EXDOSE)

# Check that each EX record is for a single dose (EXSTDTC == EXENDTC)
assert_that(all(src_100$ex$EXSTDTC == src_100$ex$EXENDTC))

# Filter the EX data to keep only subjects in DM
ex_0 <-
  src_100$ex %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

# Transmute to standard NONMEM columns:
#   AMT = dose amount, EVID = 1, MDV = 1
ex_1 <-
  ex_0 %>%
  transmute(
    USUBJID,
    AMT = EXDOSE,
    EVID = 1,
    DATETIME = ymd_hms(EXSTDTC),
    MDV = 1
  )

# Store in time-varying list
out$tv$dosing <- ex_1

# Check that we haven't coded anything as -99
assert_that(!any(out$tv$dosing == -99, na.rm = TRUE))

# -----------------------------------------------------------------------------
# 7. Combine Domains
#    We now combine:
#      - time-varying data (PC and Dosing) into one long dataset (bind_rows)
#      - subject-level data (DM, Vitals) via a left join.
# -----------------------------------------------------------------------------

# First, verify no duplicates in subject-level data
assert_that(
  all(map(out$sl, ~ !any(duplicated(.x[["USUBJID"]]))) %>% unlist())
)

# Bind the time-varying data (e.g., PC + Dosing) vertically,
# then join the subject-level data horizontally.
pk_0 <-
  bind_rows(out$tv) %>%
  left_join(
    reduce(out$sl, full_join)  # merges DM and Vitals into one dataset
  ) %>%
  arrange(USUBJID, DATETIME)

# -----------------------------------------------------------------------------
# 8. Additional NONMEM-specific derived columns
#    - FIRST_DOSE_TIME: earliest dose time per subject
#    - N_DOSES: running count of doses
#    - TIME: time since first dose
#    - TAD: time since the current dose
#    - OCC: occasion number (useful for modeling)
# -----------------------------------------------------------------------------

pk_1 <-
  pk_0 %>%
  group_by(USUBJID) %>%
  mutate(
    # earliest dosing time by subject, ignoring NAs
    FIRST_DOSE_TIME = min(DATETIME[EVID == 1], na.rm = TRUE),
    # cumulative count of dosing events
    N_DOSES = cumsum(EVID == 1),
    # replace any NAs with 0
    N_DOSES = replace_na(N_DOSES, 0),
    # time since first dose (in hours)
    TIME = as.numeric(difftime(DATETIME, FIRST_DOSE_TIME, units = "hours"))
  ) %>%
  group_by(N_DOSES, .add = TRUE) %>%
  mutate(
    # TAD = time since current dose
    TAD = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")),
    # check whether this dose interval has any PK observation
    DOSE_HAD_PK = any(EVID == 0, na.rm = TRUE)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    # detect if we've started a new dose
    NEW_DOSE = N_DOSES != lag(N_DOSES, default = first(N_DOSES)),
    # OCC increments only when we start a new dose interval that has PK data
    OCC = cumsum(NEW_DOSE & DOSE_HAD_PK) * (N_DOSES != 0)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 9. Carry forward dose amounts (if needed)
#    We fill in the DOSE column so all rows after a dose record have the same
#    amount, until a new dose is encountered. This helps in modeling setups.
# -----------------------------------------------------------------------------

pk_2 <-
  pk_1 %>%
  mutate(DOSE = AMT) %>%
  group_by(USUBJID) %>%
  # fill DOSE downwards and upwards
  fill(c("DOSE"), .direction = "downup") %>%
  ungroup()

# -----------------------------------------------------------------------------
# 10. Final columns, ID, and numeric row index
#     Prepare the final dataset for writing out.
# -----------------------------------------------------------------------------

pk_3 <-
  pk_2 %>%
  mutate(
    # ID = unique integer per subject (common in NONMEM)
    ID = as.integer(fct_inorder(USUBJID)),
    # NUM = row number across entire data set
    NUM = 1:n(),
    # Example extra column for comments or other info; blank here
    C = NA_character_
  )

# Select columns in the order specified by pk_spec
pk_out <- pk_3 %>% select(names(pk_spec))

# -----------------------------------------------------------------------------
# 11. Validation and Export
#     - Check the final data against our yspec spec
#     - Write out the dataset as a CSV
# -----------------------------------------------------------------------------

# Validate dataset structure (variable types, missingness) against the spec
ys_check(pk_out, pk_spec)

# Write the final derived dataset to disk as CSV
write_derived(
  .data = pk_out,
  .spec = pk_spec,
  .file = "data/derived/pk.csv"
)


```
