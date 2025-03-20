
# mrgda <a href='https:/metrumresearchgroup.github.io/mrgda'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml)
<!-- badges: end -->

## Example DA

``` r
# Load required libraries
library(tidyverse)
library(here)
library(assertthat)
library(yspec)
library(mrgda)
library(lubridate)

# Prepare storage list
out <- list(sl = list(), tv = list())

# Load PK specification
pk_spec <- ys_load(here("data/derived/pk.yaml"))

# Read SDTM source data
src_100 <- read_src_dir(here("data", "source", "100"))

# Prepare Demographics
out$sl$dm <- 
  src_100$dm %>%
  transmute(
    USUBJID,
    STUDYID,
    SEX = if_else(SEX == "F", 1, if_else(SEX == "M", 2, -99)),
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 3,
      RACE == "OTHER" ~ 6,
      TRUE ~ -99
    ),
    BLAGE = AGE
  )
assert_that(!any(out$sl$dm == -99, na.rm = TRUE))

# Prepare Vitals
out$sl$vs <- 
  src_100$vs %>%
  filter(
    USUBJID %in% out$sl$dm$USUBJID,
    VSBLFL == "Y",
    VSTESTCD %in% c("WEIGHT", "HEIGHT")
  ) %>%
  select(
    USUBJID,
    VSTESTCD,
    VSSTRESN
  ) %>%
  pivot_wider(
    names_from = VSTESTCD,
    values_from = VSSTRESN
  ) %>%
  transmute(
    USUBJID,
    BLWT = WEIGHT,
    BLBMI = WEIGHT / ((HEIGHT / 100)^2)
  )
assert_that(!any(duplicated(out$sl$vs$USUBJID)), !anyNA(out$sl$vs))

# Prepare PK concentrations
out$tv$pc <- 
  src_100$pc %>%
  filter(
    USUBJID %in% out$sl$dm$USUBJID,
    PCTEST == "DRUG-X",
    PCSTAT != "NOT DONE"
  ) %>%
  transmute(
    USUBJID,
    DV = PCSTRESN,
    BLQ = if_else(PCORRES == "<LLOQ", 1, 0),
    EVID = if_else(BLQ == 1, 2, 0),
    MDV = if_else(BLQ == 1, 0, 1),
    DATETIME = ymd_hms(PCDTC),
    LLOQ = PCLLOQ
  )
assert_that(!any(duplicated(out$tv$pc[, c("USUBJID", "DATETIME")])), !anyNA(out$tv$pc$DATETIME))

# Prepare dosing
out$tv$dosing <- 
  src_100$ex %>%
  filter(USUBJID %in% out$sl$dm$USUBJID) %>%
  transmute(
    USUBJID,
    AMT = EXDOSE,
    EVID = 1,
    DATETIME = ymd_hms(EXSTDTC),
    MDV = 1
  )
assert_that(!any(out$tv$dosing == -99, na.rm = TRUE))

# Prepare time-varying vitals
out$tv$wt <- 
  src_100$vs %>% 
  filter(VSTEST == "WEIGHT") %>% 
  transmute(
    USUBJID,
    WT = VSSTRESN,
    DATETIME = ymd_hms(VSDTC),
    EVID = -99
  )

# Combine domains
pk_data0 <- 
  bind_rows(out$tv) %>%
  left_join(reduce(out$sl, full_join), by = "USUBJID") %>%
  arrange(USUBJID, DATETIME)

# Fill weight and remove placeholder records
pk_data1 <- 
  pk_data0 %>% 
  group_by(USUBJID) %>%
  fill(c("WT"), .direction = "downup") %>%
  ungroup() %>% 
  filter(EVID != -99)

# Add NONMEM-specific derived columns
pk_data2 <- 
  pk_data1 %>%
  group_by(USUBJID) %>%
  mutate(
    FIRST_DOSE_TIME = min(DATETIME[EVID == 1], na.rm = TRUE),
    N_DOSES = replace_na(cumsum(EVID == 1), 0),
    TIME = as.numeric(difftime(DATETIME, FIRST_DOSE_TIME, units = "hours"))
  ) %>%
  group_by(USUBJID, N_DOSES) %>%
  mutate(
    TAD = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")),
    DOSE_HAD_PK = any(EVID == 0, na.rm = TRUE)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    NEW_DOSE = N_DOSES != lag(N_DOSES, default = first(N_DOSES)),
    OCC = cumsum(NEW_DOSE & DOSE_HAD_PK) * (N_DOSES != 0)
  ) %>%
  ungroup()

# Carry forward dose amounts
pk_data3 <- 
  pk_data2 %>%
  mutate(DOSE = AMT) %>%
  group_by(USUBJID) %>%
  tidyr::fill(DOSE, .direction = "downup") %>%
  ungroup()

# Finalize dataset
pk_data_final <- 
  pk_data3 %>%
  mutate(
    ID = as.integer(forcats::fct_inorder(USUBJID)),
    NUM = row_number(),
    C = NA_character_
  ) %>%
  select(names(pk_spec))

# Validate and export
ys_check(pk_data_final, pk_spec)
write_derived(pk_data_final, pk_spec, here("data/derived/pk.csv"))
```
