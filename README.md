
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmvalidate

A data validation tool designed to identify errors in a derived NONMEM
data set. `nm_validate` runs a series of pass/fail checks on a data set
and `nm_summary` provides a quick visualization of the data.

## Background

Often data sets are derived according to a data specification file. This
package leverages the information within this file to validate the data
set. It is suggested that the data specification file be made in `yaml`
format. An example of one can be found in `inst/derived/pk.yml`.

Within the setup section, `flags` are defined. In order to use
`nmvalidate` the names of the flags must match those shown below:

``` r
flags:
  # Unique subject ID
  id: [ID]
  # Study ID
  study: [STUDYID]
  # Columns that define a unique row (other than ID and TIME) - typically EVID, etc
  primary_keys: [EVID, DVID] 
  # Time identifier
  time: [TIME] 
  # Baseline categorical covariates
  bl_cov_cat: [SEX, RACE, BLADA] 
  # Baseline continuous covariates
  bl_cov_cont: [BLWT, BLALB] 
  # Time-varying categorical covariates
  tv_cov_cat: [ADA] 
  # Time-varying continuous covariates
  tv_cov_cont: [WT, ALB] 
```

*IMPORTANT NOTE* - it is not necessary to have a variable provided for
each flag. Adding variables to each of the flags is optional, however
the significance of the output will be decreased with the less
information provided in flags.

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/nmvalidate/>

## Development

`nmvalidate` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
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

## Package coverage

``` r
covr::package_coverage()
#> nmvalidate Coverage: 94.55%
#> R/nm-summary.R: 87.60%
#> R/gather-flags.R: 100.00%
#> R/nm-validate.R: 100.00%
```
