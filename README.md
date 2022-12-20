
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

mrgda is a NONMEM data assembly helper, providing a set of functions
that help validate your derived data set. Working hand in hand with your
data specification, `nm_validate()` checks for common data set errors
such as:

-   Duplicated events
-   Non-unique baseline covariates
-   Missing covariates

To use mrgda optimally, you are encouraged to set up your data
specification file in `yaml` format, similar to the example found
[here](https://github.com/metrumresearchgroup/mrgda/blob/main/inst/derived/pk.yml)
and discussed further in the Setup section below.

## Setup

Before running mrgda, a `flags` parameter needs to be defined within the
`SETUP` section of the data specification yaml file. All relevant column
names need to be defined for each of the following flags:

-   `id` - subject identification (typically ID)
-   `study` - study identification
-   `primary_keys` - event defining variables (ie. EVID and DVID)
-   `time` - time
-   `bl_cat_cov` - baseline categorical covariates
-   `tv_cat_cov` - time-varying categorical covariates
-   `bl_cont_cov` - baseline continuous covariates
-   `tv_cont_cov` - time-varying continuous covariates

Note in the example below that multiple variables can be listed for each
flag and that not all flags need to be defined. For instance, no
time-varying categorical covariates are listed below.

``` r
SETUP:
  flags:
    id: [ID]
    study: [STUDYID]
    primary_keys: [EVID, DVID]
    time: [TIME]
    bl_cat_cov: [SEX, RACE]
    bl_cont_cov: [WTBL, BMIBL, AGEBL] 
    tv_cont_cov: [WT]
```

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
