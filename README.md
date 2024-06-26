
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrgda <a href='https:/metrumresearchgroup.github.io/mrgda'><img src='man/figures/logo.png' align="right" width="135px"/></a>

<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgda/actions/workflows/main.yaml)
<!-- badges: end -->

## Overview

`mrgda` is a data assembly helper, providing a set of functions that
help you assemble, explore and validate your data set.

### Featured Vignettes

-   [Getting
    Started](https://metrumresearchgroup.github.io/mrgda/articles/getting-started.html)
    – Introduction to commonly used data assembly `mrgda` functions.
-   [Assigning
    IDs](https://metrumresearchgroup.github.io/mrgda/articles/assigning-id.html)
    – Background on `assign_id()`. `assign_id()` helps you assign a
    unique numeric identifier to each subject in your data set. Its
    purpose is to ensure the same numeric identifier is consistently
    assigned to each subject throughout a project

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/mrgda/>

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[mrgda](https://github.com/metrumresearchgroup/mrgda/issues).

#### Developer Notes

<details closed>
<summary>
Developer Notes
</summary>

`mrgda` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
manage development dependencies and
[renv](https://rstudio.github.io/renv/) to provide isolation. To
replicate this environment,

1.  clone the repo

2.  install [pkgr](https://github.com/metrumresearchgroup/pkgr)

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` prior to
        this step if not already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). `renv` will activate and find the project library.

</details>
