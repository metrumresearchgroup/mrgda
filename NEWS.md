# mrgda (development)

## Bug fixes

- `nm_write()` now allows for special characters in the file name.

# mrgda 0.3.1

## Bug fixes

- `read_src_dir()` now allows for source directories in which all files have no USUBJID.

# mrgda 0.3.0

## New features and changes

- User can now omit tests from being run in `nm_validate()`. Optional argument
  provided to do this. (#38)

- `nm_validate()` and `nm_summary()` now utilize a dictionary of column names 
  found here `system.file("package-data", "recognized-flags.csv", package = "mrgda")`. 
  The functions can extract information from the data set without the user 
  explicitly needing to define these columns. (#38)
  
- `view_sdtm_domains()` allows the user to view a dictionary of common SDTM 
  domains and a description of the data found within them. (#38)
  
- `read_src_dir()` reads in all .csv, .xpt or .sas7bdat files from a source data
  directory into a nested list containing the data from each domain.(#38)

- `read_src_dir()` creates an additional data set informing the user if a subject 
  has data in each domain. The resulting data.frame has one row per subject with
  a TRUE/FALSE column indicating if they are in a specific domain. (#38)

- `nm_write()` writes out derived data to a .csv file and creates a folder matching
  the name of the derived data, containing meta data from the assembly. (#31, #36, #38)

- `nm_validate()` added 4 new pass/fail checks. These include checking for: 
  non-finite times, MDV not set to 1 when DV is NA, all NUM values being unique
  and AMT being equal to RATE times DUR. (#38)

- `nm_summary()` now opens an html document in a new tab containing both
  tables and figures. The figures are now interactive, allowing the user to hover
  over data points and view subject level information. (#30)

- `nm_summary()` added a table showing the distribution of BLQ values in the data.
  Additionally, new spaghetti plots were added to view time-varying covariates. (#30)
  
- `nm_summary()` now allows the user to decide if the tables and boxplots are to
  be stratified by study or not. (#30)

- `mutate_egfr()` was removed from the package. (#38)

## Bug fixes

- `nm_validate()` could not run when a data set did not have the needed data 
  columns to run a specific check. Protections were added that allow tests to be
  skipped if the required data columns are not present. (#38)


# mrgda 0.2.1

## Bug fixes

- `nm_validate` did not always print the corresponding test name alongside the 
  debug code. (#16)


# mrgda 0.2.0

## New features and changes

- Output of `nm_validate()` now returns code using the users given arguments
  when errors are found in the data. The code helps the user to debug the
  issue. (#12)

- Two checks in `nm_validate()` were combined. The missing time varying covariate
  and baseline covariate checks are now combined as a missing covariate check. (#12)

- Baseline and time-varying covariate flag names were updated to make them
  more easily readable. The updated flag names are available in the README. (#9)

- `mutate_egfr()` was added as a new function to assist users calculating
  estimated glomerular filtration rate during data assemblies (#8)

- `readr` and `withr` were moved to suggests from imports. `assertr` was removed
  as a dependency. (#4, #12)

## Bug fixes

- `nm_summary()` did not create readable continuous and categorical covariate 
  figures when a large number of covariates were present. (#2)


# mrgda 0.1.0

## New features and changes

- `mrgda` initial aim is to assist users in verifying the accuracy
  of data sets intended for NONMEM, following their derivation.

- The main feature, `nm_validate()` asks the user to provide the derived
  data set and spec file as inputs. The function identifies columns in the
  data set using the spec file and runs a series of validation tests. It outputs
  a pass/fail for each test.
  
- A supporting feature is `nm_summary()`. Its purpose is to provide the user
  with quick visual summaries of their data. It outputs these summaries in
  either tables or figures, depending on the users preference.

