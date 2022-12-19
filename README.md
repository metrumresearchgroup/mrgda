
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

`mrgda` assists with deriving and validating NONMEM data sets. While
assembling data, a set of functions are provided to mutate common NONMEM
variables:

-   `mutate_egfr()` adds an estimated glomerular filtration rate (eGFR)
    column

Once derived, `mrgda` will help you identify any common errors that
exist in the data, by providing:

-   `nm_summary()` outputs covariate summary tables and informative data
    visualizations

-   `nm_validate()` runs a series of pass/fail checks for:

    -   Duplicated events
    -   Non-unique baseline covariates
    -   Missing covariates

These all leverage information within a data specification file. It is
suggested that this file be in `yaml` format, similar to the example
found
[here](https://github.com/metrumresearchgroup/mrgda/blob/main/inst/derived/pk.yml).
You can learn more about setting up the data specification file in the
[Getting Started with
mrgda](https://metrumresearchgroup.github.io/mrgda/articles/getting-started.html)
vignette.

## Usage

#### Data validation

`nm_validate()` outputs the results of the pass/fail checks. If a check
fails, the user will be provided code to help identify where the error
is in the data.

``` r
library(mrgda)

nm_validate(.data = nm_errors, .spec = nm_spec, .error_on_fail = FALSE)
```

    ── nm_validate() results: ──────────────────────────────────────────────────────

    ✖ No duplicate primary keys -- Copy/paste and run the following code:

    nm_errors %>%
     dplyr::select(ID, TIME, EVID, DVID) %>%
     dplyr::count(ID, TIME, EVID, DVID) %>%
     dplyr::filter(n > 1)

    ✖ Non-unique baseline covariates -- Copy/paste and run the following code:

    nm_errors %>%
     dplyr::select(ID, SEX, RACE, WTBL, BMIBL, AGEBL) %>%
     dplyr::filter(complete.cases(.)) %>%
     dplyr::distinct() %>%
     dplyr::group_by(across(ID)) %>%
     dplyr::add_count() %>%
     dplyr::ungroup() %>%
     dplyr::filter(n > 1)

    ✖ No missing covariates -- Copy/paste and run the following code:

    nm_errors %>%
     dplyr::select(ID, SEX, RACE, WTBL, BMIBL, AGEBL, WT) %>%
     dplyr::filter(!complete.cases(.))

    [ FAIL 3 | PASS 0 ]

#### Data derivation

Data assembly functions such as `mutate_egfr()` mutates a new column
onto the data. The user is also provided the source for the equation
used in the caluclation.

``` r
nm_ex_df %>% 
  mutate_egfr(
    .age = AGE,
    .wt = WT,
    .serum_creatinine = SC,
    .sex = SEX,
    .female_value = 1
  )
```

    ┌ calc_egfr() formula: ─────────────────────────────────────────────────────────────┐
    │                                                                                   │
    │   function (.age, .wt, .serum_creatinine, .sex, .female_value)                    │
    │   {                                                                               │
    │       .alpha <- dplyr::if_else(.sex == .female_value, -0.241, -0.302)             │
    │       .k <- dplyr::if_else(.sex == .female_value, 0.7, 0.9)                       │
    │       142 * (min(.serum_creatinine/.k, 1)^.alpha) * (max(.serum_creatinine/.k,    │
    │           1)^-1.2) * (0.9938^.age) * (dplyr::if_else(.sex == .female_value,       │
    │           1.012, 1))                                                              │
    │   }                                                                               │
    │                                                                                   │
    └───────────────────────────────────────────────────────────────────────────────────┘

    # A tibble: 3 × 6
        AGE    WT    SC   SEX  RACE  EGFR
      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    1    45    64  1.02     1     1  69.1
    2    82    23  1.04     2     2  71.7
    3    73    92  1.98     1     3  26.2

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/mrgda/>

## Development

`mrgda` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
manage development dependencies and
[renv](https://rstudio.github.io/renv/) to provide isolation. To
replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [mrgda](https://github.com/mrgda/issues).
