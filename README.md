
# mrgda <a href='https:/metrumresearchgroup.github.io/mrgda'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml)
<!-- badges: end -->

## Overview

This document demonstrates how to assemble a pharmacokinetics (PK)
dataset from multiple *SDTM* source domains. We will produce a final CSV
file suitable for NONMEM analyses. The document is structured into clear
steps:

1.  **Load libraries**  
2.  **Load the PK data specification (from `yspec`)**  
3.  **Read in the SDTM data**  
4.  **Prepare demographics**  
5.  **Prepare vitals (weight, height, BMI)**  
6.  **Prepare PK concentrations**  
7.  **Prepare dosing data**  
8.  **Combine domains**  
9.  **Add NONMEM-specific derived columns**  
10. **Carry forward dose amounts**  
11. **Finalize and export**

Let’s get started.

------------------------------------------------------------------------

## 1. Load Required Libraries

We use a number of libraries for data manipulation, file path
management, assertion checks, reading/writing standardized data, and
more.

``` r
library(tidyverse)  # For data manipulation and piping
library(here)       # For managing file paths
library(assertthat) # For simple assertion checks
library(yspec)      # For handling dataset specifications
library(mrgda)      # For reading/writing data from standardized directories
library(lubridate)  # For working with dates/times
```

We’ll also prepare a list (`out`) that will store our *subject-level*
(sl) and *time-varying* (tv) data as we go.

``` r
# Prepare a list to store data pieces
out <- list(
  sl = list(),  # subject-level data
  tv = list()   # time-varying data
)
```

------------------------------------------------------------------------

## 2. Load the PK Data Specification

Our final dataset must adhere to a schema defined in a `yspec` YAML
file. This file details which columns we expect, along with labels,
units, and other metadata.

``` r
pk_spec <- ys_load(here("data/derived/pk.yaml"))
```

------------------------------------------------------------------------

## 3. Read in the Source Data

Let’s read all the SDTM data for a study called “100.” Each domain (like
`dm`, `vs`, `pc`, `ex`) is automatically placed in a named list. For
example, `src_100$dm` is the Demographics domain, `src_100$pc` is the
Concentrations domain, etc.

``` r
src_100 <- read_src_dir(here("data", "source", "100"))
```

------------------------------------------------------------------------

## 4. Prepare Demographics (DM Domain)

We want to transform certain demographic variables into numeric codes,
rename them for our final dataset, and do some quick checks.

``` r
# Inspect a few demographic variables:
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
    
    # Convert SEX from 'F'/'M' to numeric (1 or 2); -99 for unexpected
    SEX = case_when(
      SEX == "F" ~ 1,
      SEX == "M" ~ 2, 
      TRUE ~ -99
    ),
    
    # Convert RACE to numeric categories; -99 if not recognized
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

# Check that we did not unintentionally code any row as -99
assert_that(!any(out$sl$dm == -99, na.rm = TRUE))
```

------------------------------------------------------------------------

## 5. Prepare Vitals (VS Domain)

From the vitals data, we’ll collect baseline weight and height, then
compute BMI.

``` r
# Quick summary of test codes, baseline flags, and units
src_100$vs %>% 
  count(VSTESTCD, VSBLFL, VSSTRESU)

# Filter vitals data to only those in DM
vs_0 <-
  src_100$vs %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

# We'll focus on WEIGHT and HEIGHT at baseline
vs_tests <- c("WEIGHT", "HEIGHT")

vs_1 <-
  vs_0 %>%
  filter(
    VSBLFL == "Y",            # baseline records only
    VSTESTCD %in% vs_tests    # only weight & height
  ) %>%
  select(USUBJID, VSTESTCD, VSSTRESN) %>%
  pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN) %>%
  transmute(
    USUBJID,
    BLWT = WEIGHT, 
    BLBMI = WEIGHT / ((HEIGHT / 100) ^ 2)
  )

out$sl$vs <- vs_1

# Ensure no duplicates or missing data
assert_that(!any(duplicated(out$sl$vs$USUBJID)))
assert_that(!anyNA(out$sl$vs))
```

------------------------------------------------------------------------

## 6. Prepare PK Concentrations (PC Domain)

We’ll focus on a single analyte called `"DRUG-X"`, convert
below-limit-of-quantification (BLQ) flags, and set columns like `EVID`
and `MDV` for NONMEM.

``` r
# Quick look at test names and statuses in PC
src_100$pc %>% 
  count(PCTEST, PCSTAT)

# Rows missing numeric results
src_100$pc %>% 
  filter(is.na(PCSTRESN)) %>% 
  count(PCTEST, PCORRES, PCSTAT)

# Filter to keep only relevant PK data
pc_0 <-
  src_100$pc %>%
  filter(
    USUBJID %in% out$sl$dm$USUBJID,
    PCTEST == "DRUG-X",
    PCSTAT != "NOT DONE"
  )

# Create final PK columns
pc_1 <-
  pc_0 %>%
  transmute(
    USUBJID,
    DV = PCSTRESN,  # observed concentration
    BLQ = case_when(PCORRES == "<LLOQ" ~ 1, TRUE ~ 0),
    # EVID=2 for BLQ; otherwise 0
    EVID = case_when(BLQ == 1 ~ 2, TRUE ~ 0),
    # MDV=0 for BLQ (we keep the observation), else 1
    MDV = case_when(BLQ == 1 ~ 0, TRUE ~ 1),
    DATETIME = ymd_hms(PCDTC),
    LLOQ = PCLLOQ
  )

# Some quick checks
pc_1 %>% count(is.na(DV), BLQ)

out$tv$pc <- pc_1

# Verify uniqueness of (USUBJID, DATETIME) pairs
assert_that(
  !any(duplicated(out$tv$pc[, c("USUBJID", "DATETIME")])),
  !anyNA(out$tv$pc$DATETIME)
)
```

------------------------------------------------------------------------

## 7. Prepare Dosing (EX Domain)

Dosing records typically have `EVID=1` and `MDV=1` in NONMEM. We also
confirm that each record has a start and end time that match.

``` r
# Quick look at EX domain fields
src_100$ex %>% 
  count(EXTRT, EXDOSFRM, EXDOSFRQ, EXDOSU, EXDOSE)

# Check if each record is for a single dose event
assert_that(all(src_100$ex$EXSTDTC == src_100$ex$EXENDTC))

# Filter EX data to keep only subjects in DM
ex_0 <- src_100$ex %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

# Convert to standard NONMEM columns
ex_1 <- ex_0 %>%
  transmute(
    USUBJID,
    AMT = EXDOSE,
    EVID = 1,
    DATETIME = ymd_hms(EXSTDTC),
    MDV = 1
  )

out$tv$dosing <- ex_1

# Check that we haven't coded anything as -99 in dosing
assert_that(!any(out$tv$dosing == -99, na.rm = TRUE))
```

------------------------------------------------------------------------

## 8. Combine Domains

Now, we combine time-varying data (PC + dosing) into one dataset and
then left-join the subject-level (demographics and vitals) information.

``` r
# Ensure no duplicates in subject-level data
assert_that(
  all(
    purrr::map_lgl(
      out$sl, 
      ~ !any(duplicated(.x[["USUBJID"]]))
    )
  )
)

# Bind time-varying data vertically, then join subject-level data
pk_0 <-
  bind_rows(out$tv) %>%
  left_join(
    purrr::reduce(out$sl, full_join),
    by = "USUBJID"
  ) %>%
  arrange(USUBJID, DATETIME)
```

------------------------------------------------------------------------

## 9. Add NONMEM-Specific Derived Columns

We add columns such as:

-   `FIRST_DOSE_TIME`: the earliest dose time per subject
-   `N_DOSES`: running count of doses
-   `TIME`: time since first dose
-   `TAD`: time since current dose
-   `OCC`: occasion number (for modeling)

``` r
pk_1 <-
  pk_0 %>%
  group_by(USUBJID) %>%
  mutate(
    FIRST_DOSE_TIME = min(DATETIME[EVID == 1], na.rm = TRUE),
    N_DOSES = cumsum(EVID == 1),
    N_DOSES = replace_na(N_DOSES, 0),
    TIME = as.numeric(difftime(DATETIME, FIRST_DOSE_TIME, units = "hours"))
  ) %>%
  group_by(N_DOSES, .add = TRUE) %>%
  mutate(
    TAD = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")),
    DOSE_HAD_PK = any(EVID == 0, na.rm = TRUE)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    NEW_DOSE = N_DOSES != dplyr::lag(N_DOSES, default = first(N_DOSES)),
    OCC = cumsum(NEW_DOSE & DOSE_HAD_PK) * (N_DOSES != 0)
  ) %>%
  ungroup()
```

------------------------------------------------------------------------

## 10. Carry Forward Dose Amounts

To facilitate some modeling approaches and visualizations, we fill in
`DOSE` so that observations following a dose record inherit the same
amount.

``` r
pk_2 <-
  pk_1 %>%
  mutate(DOSE = AMT) %>%
  group_by(USUBJID) %>%
  tidyr::fill(c("DOSE"), .direction = "downup") %>%
  ungroup()
```

------------------------------------------------------------------------

## 11. Finalize and Export the Dataset

We assign unique integer IDs per subject, add a numeric row index, and
then reorder columns according to our specification (`pk_spec`).
Finally, we do a `yspec` validation and write to disk as CSV.

``` r
pk_3 <-
  pk_2 %>%
  mutate(
    ID = as.integer(forcats::fct_inorder(USUBJID)),
    NUM = 1:n(),
    C = NA_character_
  )

# Reorder columns to match the spec
pk_out <- pk_3 %>% select(names(pk_spec))

# Validate
ys_check(pk_out, pk_spec)

# Write the final derived dataset to disk
write_derived(
  .data = pk_out,
  .spec = pk_spec,
  .file = "data/derived/pk.csv"
)
```
