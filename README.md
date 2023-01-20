
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

mrgda is a NONMEM data assembly helper, providing a set of functions
that help you assemble, explore and validate your data set.

To use mrgda optimally, you are encouraged to set up your data
specification file in `yaml` format, similar to the example found
[here](https://github.com/metrumresearchgroup/mrgda/blob/main/inst/derived/pk.yml)
and discussed further in the `Setup` section below.

## Setup

`nm_validate()` and `nm_summary()` extract knowledge from your data
through the use of column flags that can be setup in your data
specification file. Some flags change project to project, such as ones
defining covariates. These need to be defined in the data specification
file as such:

``` r
SETUP:
  flags:
    bl_cat_cov: [SEX, RACE]
    bl_cont_cov: [WTBL, BMIBL, AGEBL]
    tv_cat_cov: [HEPAT]
    tv_cont_cov: [WT]
```

Every column name that fits each category is listed as shown above. The
definitions for these flags are as such:

-   `bl_cat_cov` - baseline categorical covariates
-   `bl_cont_cov` - baseline continuous covariates
-   `tv_cat_cov` - time-varying categorical covariates
-   `tv_cont_cov` - time-varying continuous covariates

Other columns will also be utilized, so it is recommended to use the
following column names in your data:

<div id="ehrdyzftcp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ehrdyzftcp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ehrdyzftcp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ehrdyzftcp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ehrdyzftcp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ehrdyzftcp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehrdyzftcp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ehrdyzftcp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ehrdyzftcp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ehrdyzftcp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ehrdyzftcp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ehrdyzftcp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ehrdyzftcp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ehrdyzftcp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ehrdyzftcp .gt_from_md > :first-child {
  margin-top: 0;
}

#ehrdyzftcp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ehrdyzftcp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ehrdyzftcp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ehrdyzftcp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ehrdyzftcp .gt_row_group_first td {
  border-top-width: 2px;
}

#ehrdyzftcp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehrdyzftcp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ehrdyzftcp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ehrdyzftcp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehrdyzftcp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehrdyzftcp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ehrdyzftcp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ehrdyzftcp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehrdyzftcp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ehrdyzftcp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehrdyzftcp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ehrdyzftcp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehrdyzftcp .gt_left {
  text-align: left;
}

#ehrdyzftcp .gt_center {
  text-align: center;
}

#ehrdyzftcp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ehrdyzftcp .gt_font_normal {
  font-weight: normal;
}

#ehrdyzftcp .gt_font_bold {
  font-weight: bold;
}

#ehrdyzftcp .gt_font_italic {
  font-style: italic;
}

#ehrdyzftcp .gt_super {
  font-size: 65%;
}

#ehrdyzftcp .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ehrdyzftcp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ehrdyzftcp .gt_indent_1 {
  text-indent: 5px;
}

#ehrdyzftcp .gt_indent_2 {
  text-indent: 10px;
}

#ehrdyzftcp .gt_indent_3 {
  text-indent: 15px;
}

#ehrdyzftcp .gt_indent_4 {
  text-indent: 20px;
}

#ehrdyzftcp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col">Flag name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col">Column name</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">id</td>
<td class="gt_row gt_center">ID</td></tr>
    <tr><td class="gt_row gt_center">dv</td>
<td class="gt_row gt_center">DV</td></tr>
    <tr><td class="gt_row gt_center">amt</td>
<td class="gt_row gt_center">AMT</td></tr>
    <tr><td class="gt_row gt_center">study</td>
<td class="gt_row gt_center">STUDYID or STUDY</td></tr>
    <tr><td class="gt_row gt_center">time</td>
<td class="gt_row gt_center">TAFD or TIME</td></tr>
    <tr><td class="gt_row gt_center">num</td>
<td class="gt_row gt_center">NUM</td></tr>
    <tr><td class="gt_row gt_center">mdv</td>
<td class="gt_row gt_center">MDV</td></tr>
    <tr><td class="gt_row gt_center">evid</td>
<td class="gt_row gt_center">EVID</td></tr>
    <tr><td class="gt_row gt_center">dvid</td>
<td class="gt_row gt_center">DVID</td></tr>
    <tr><td class="gt_row gt_center">blq</td>
<td class="gt_row gt_center">BQL or BLQ</td></tr>
    <tr><td class="gt_row gt_center">occ</td>
<td class="gt_row gt_center">OCC</td></tr>
    <tr><td class="gt_row gt_center">rate</td>
<td class="gt_row gt_center">RATE</td></tr>
    <tr><td class="gt_row gt_center">dur</td>
<td class="gt_row gt_center">DUR</td></tr>
  </tbody>
  
  
</table>
</div>

If you have a column name that is different than the expected one shown
above, you can define it in your specification file as shown below. Here
we are defining our `study` column name as `STUDYN`.

``` r
SETUP:
  flags:
    study: [STUDYN]
    bl_cat_cov: [SEX, RACE]
    bl_cont_cov: [WTBL, BMIBL, AGEBL]
    tv_cat_cov: [HEPAT]
    tv_cont_cov: [WT]
```

## Usage

### Data validation

To validate your data, simply provide `nm_validate()` with your NONMEM
data set and data specification. It’s output will indicate the result of
a series of pass/fail validation checks.

``` r
library(mrgda)
nm_validate(.data = nm_final_corrected, .spec = nm_spec, .error_on_fail = FALSE)
```

    ── nm_validate() results: ──────────────────────────────────────────────────────

    ✔ No duplicate records

    ✔ Non-unique baseline covariates

    ✔ No missing covariates

    ✔ Non-finite TIME values

    ✔ MDV not set to 1 when DV is NA

    ✔ All NUM values are unique

    ✔ All dosing AMT values are equivalent to RATE * DUR


    [ FAIL 0 | SKIP 0 | PASS 7 ]

If an error is found in the data, you will be provided with code to help
debug where the problem occurs.

``` r
nm_validate(.data = nm_final, .spec = nm_spec, .error_on_fail = FALSE)
```

    ## 

    ## ── nm_validate() results: ──────────────────────────────────────────────────────

    ## ✔ No duplicate records

    ## ✔ Non-unique baseline covariates

    ## ✔ No missing covariates

    ## ✔ Non-finite TIME values

    ## ✔ MDV not set to 1 when DV is NA

    ## ✔ All dosing AMT values are equivalent to RATE * DUR

    ## ✖ All NUM values are unique -- Copy/paste and run the following code:

    ## nm_final %>%
    ##  dplyr::select(NUM) %>%
    ##  dplyr::group_by(across(NUM)) %>%
    ##  dplyr::add_count() %>%
    ##  dplyr::ungroup() %>%
    ##  dplyr::filter(n > 1)
    ## 
    ## [ FAIL 1 | SKIP 0 | PASS 6 ]

### Data summary

To explore your data, provide `nm_summary()` with your NONMEM data set
and data specification. A html document will be generated with tables
and figures to help you visualize your data.

``` r
nm_summary(.data = nm, .spec = nm_spec)
```

![Baseline continuous covariates
table](man/figures/bl-cont-cov-table.png)

![Baseline continuous covariates figure](man/figures/cov-boxplot.png)

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
