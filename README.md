
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrgda <a href='https:/metrumresearchgroup.github.io/mrgda'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml)
<!-- badges: end -->

```r
library(tidyverse)
library(here)
library(assertthat)

# Prepare to save individual domain assembly pieces
# sl: subject level
# tv: time varying
out <- list(sl = list(), tv = list())

# Spec --------------------------------------------------------------------
pk_spec <- yspec::ys_load(here("data/derived/pk.yaml"))

# Source data -------------------------------------------------------------
src_100 <- mrgda::read_src_dir(here::here("data", "source", "100"))

# Demographics ------------------------------------------------------------
src_100$dm %>% pivot_longer(c(ACTARM, SEX, RACE, ETHNIC)) %>% count(name, value)

dm_1 <-
  src_100$dm %>%
  transmute(
    USUBJID,
    STUDYID,
    SEX = case_when(
      SEX == "F" ~ 1,
      SEX == "M" ~ 2, 
      TRUE ~ -99
    ),
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 3,
      RACE == "OTHER" ~ 6,
      TRUE ~ -99
    ),
    BLAGE = AGE
  )

out$sl$dm <- dm_1

assert_that(!(any(out$sl$dm == -99, na.rm = TRUE)))

# Vitals ------------------------------------------------------------------
src_100$vs %>% count(VSTESTCD, VSBLFL, VSSTRESU)

vs_0 <-
  src_100$vs %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

vs_tests <- c("WEIGHT", "HEIGHT")

vs_1 <-
  vs_0 %>%
  filter(
    VSBLFL == "Y",
    VSTESTCD %in% vs_tests
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
    BLBMI = WEIGHT / ((HEIGHT / 100) ^ 2)
  )

out$sl$vs <- vs_1

assert_that(!any(duplicated(out$sl$vs$USUBJID)))
assert_that(!(anyNA(out$sl$vs)))

# PK ----------------------------------------------------------------------
src_100$pc %>% count(PCTEST, PCSTAT)
src_100$pc %>% filter(is.na(PCSTRESN)) %>% count(PCTEST, PCORRES, PCSTAT)

pc_0 <-
  src_100$pc %>%
  filter(
    USUBJID %in% out$sl$dm$USUBJID,
    PCTEST == "DRUG-X",
    PCSTAT != "NOT DONE"
  )

pc_1 <-
  pc_0 %>%
  transmute(
    USUBJID,
    DV = PCSTRESN,
    BLQ = case_when(PCORRES == "<LLOQ" ~ 1, TRUE ~ 0),
    EVID = case_when(BLQ == 1 ~ 2, TRUE ~ 0),
    MDV = case_when(BLQ == 1 ~ 0, TRUE ~ 1),
    DATETIME = ymd_hms(PCDTC),
    LLOQ = PCLLOQ
  )

pc_1 %>% count(is.na(DV), BLQ)

out$tv$pc <- pc_1

assert_that(
  !any(duplicated(out$tv$pc[, c("USUBJID", "DATETIME")])),
  !anyNA(out$tv$pc$DATETIME)
)

# Dosing ------------------------------------------------------------------
src_100$ex %>% count(EXTRT, EXDOSFRM, EXDOSFRQ, EXDOSU, EXDOSE)

# Ensure each row is a single dose
assert_that(all(src_100$ex$EXSTDTC == src_100$ex$EXENDTC))

ex_0 <-
  src_100$ex %>%
  filter(USUBJID %in% out$sl$dm$USUBJID)

ex_1 <-
  ex_0 %>%
  left_join(out$sl$dm %>% select(USUBJID, ACTARM)) %>%
  left_join(out$sl$vs %>% select(USUBJID, WT))

ex_2 <-
  ex_1 %>%
  transmute(
    USUBJID,
    AMT = EXDOSE,
    EVID = 1,
    DATETIME = ymd_hms(EXSTDTC),
    MDV = 1
  )

out$tv$dosing <- ex_2

assert_that(!any(out$tv$dosing == -99, na.rm = TRUE))

# Combine Domains ---------------------------------------------------------
assert_that(
  all(map(out$sl, ~ !any(duplicated(.x[["USUBJID"]]))) %>% unlist())
)

pk_0 <-
  bind_rows(out$tv) %>%
  left_join(
    reduce(out$sl, full_join)
  ) %>%
  arrange(USUBJID, DATETIME)

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
    NEW_DOSE = N_DOSES != lag(N_DOSES, default = first(N_DOSES)),
    OCC = cumsum(NEW_DOSE & DOSE_HAD_PK) * (N_DOSES != 0)
  ) %>%
  ungroup()

pk_2 <-
  pk_1 %>%
  mutate(DOSE = AMT) %>%
  group_by(USUBJID) %>%
  fill(c("DOSE"), .direction = "downup") %>%
  ungroup()

pk_3 <-
  pk_2 %>%
  mutate(
    ID = as.integer(fct_inorder(USUBJID)),
    NUM = 1:n(),
    C = NA_character_
  )

pk_out <- pk_3 %>% select(names(pk_spec))

# Write dataset -----------------------------------------------------------
yspec::ys_check(pk_out, pk_spec)

mrgda::write_derived(
  .data = pk_out,
  .spec = pk_spec,
  .file = "data/derived/pk.csv"
)
```
