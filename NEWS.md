# mrgda 0.11.1

## Bug fixes

- `write_derived()` now checks that the given subject column exists in the data. (#194)

- `write_derived()` now performs a spec check before writing out data. (#196)

# mrgda 0.11.0

## New features and changes

- Renamed `query_src_dir` to `query_src_list`, and it now takes a list as an argument. (#182)

## Bug fixes

- `assign_id()` is now protected against grouped data.frames. (#188)

- Fixed `write_derived()` warning message when creating `history.csv`. (#186)

# mrgda 0.10.0

## New features and changes

- Modified the `write_derived` function to save data history in the meta data folder (`history.csv`). (#171, #176)

- Created the `explain()` function that communicates code intentionality. (#164)

# mrgda 0.9.0

## New features and changes

- renamed function `view_src()` to `v()`, and function received a styling overhaul. (#141)

- `execute_data_diffs` was made to run faster. (#142)

- `nm_validate()` and `nm_write()` was removed from the package. (#156)

- `write_derived()` no longer writes out the subject level csv in the meta data folder. (#158)

- added `assigning-id` vignette. (#160)

## Bug fixes

- `src_viz()` app bug fixes and improvements with handling subject columns. (#141)

- `assign_id()` now assigns ID's based on maximum ID in previous data set, even if no subjects overlap. (#158)

# mrgda 0.8.1

## Bug fixes

- `execute_data_diffs()` Since the NUM column is excluded from subject level diffs, it needed to be removed from the list of columns       used for the subject level diffs. (#138)

# mrgda 0.8.0

## New features and changes

- add `check-src-duplicates.R` function to the package to perform source data duplicate checks. (#113)

- add `check-src-missing-datetime.R` function to the package to check for missing values. (#113)

- add `check-src.R` to run all source data checks. (#113)

- add `src-viz.R` function to the package to view all source domains in a shiny app. (#120)

- `write-derived.R` now always explicitly exports the HEAD version from svn for data diffs. (#126)

- `write-derived.R` now arranges subject-level data by numerical ID. (#126)

- `write-derived.R` now always writes out subject level diffs even if they are empty. (#135)

- `execute-data-diffs.R` now returns a list of diffs and does not write out. (#135)

- `assign-id.R` prints a simplified message. (#136)

## Bug fixes

- `write-derived.R` The printed message for columns "added" or "removed" is now correct. (#126)

# mrgda 0.7.2

## Bug fixes

- `assign_id()` now does not create duplicate records. (#110)

# mrgda 0.7.1

## Bug fixes

- `nm_write()` N IDs diff now shows number of IDs that changed. (#108)

# mrgda 0.7.0

## New features and changes

- `assign_id()` now only outputs console message when new ID's present. (#104)

- `read_src_dir()` no longer outputs subject column domain. (#106)

- `query_src_dir()` returns a data.frame now instead of a view of the output. (#106)

# mrgda 0.6.2

## New features and changes

- `write_derived()` now shows number of IDs change now displayed in diff. (#102)

# mrgda 0.6.1

## New features and changes

- `write_derived()` now allows users to turn off executing diffs. (#100)

## Bug fixes

- `write_derived()` fixed error created by different classes in ID diff. (#100)

# mrgda 0.6.0 

## New features and changes

- Removed `nm_summary()`. (#81)

- `nm_write()` now pulls the base data to compare to from svn. (#83)

- `nm_write()` now runs additional diff checks at the ID level. (#83)

- Add `get_data_version()` to assist with data versioning. (#87)

- Add `write_derived()` to write out csv files along with meta information about the data. (#87)

- `view_mrgda_flags()` was removed from the package. (#87)

- `write_derived()` print messages were cleaned up. (#92, #97)

- `assign_id()` function added to package. (#93)

## Bug fixes

- `read_src_dir()` fixed error message when source domains fail to read in. (#81)

# mrgda 0.5.2

## Bug fixes

- `nm_write()` no longer renders a define document for the data specification. (#75)

# mrgda 0.5.1

## New features and changes

- Added `view_mrgda_flags()` to allow the user to view a summary of how mrgda flags in the specification file are assigned to the data set. (#67)

- `read_src_dir()` now outputs the name and size of the current file its reading in. (#67)

- `query_src_dir()` now allows the user to specify file types. (#67)

## Bug fixes

- `nm_summary()` now will only run if all required flags are available. (#67)

- `nm_validate()` has stricter requirements for the data it needs defined to run each check. (#67)

- `read_src_dir()` Allows non-detect method for file type discovery. (#67)


# mrgda 0.5.0

## New features and changes

- `query_src_dir()` is a new function that searches through the data in a source directory for a string or pattern. (#64)

- `distinct_subject_columns()` is a new function that takes a data frame and subject column identifier and returns the columns in which the values are unique for every subject. (#64)

- `nm_write()` the experimental feature of including the data assembly source script in the meta data folder has been removed. (#64)

- `read_src_dir()` now returns an additional data object - a data frame containing the column name and labels from every domain. (#64)

# mrgda 0.4.0

## New features and changes

- `nm_validate()` now checks if MDV is set to 1 when DV is either NA or 0. (#55)

- `nm_write()` now determines and saves out the names of other analysis that 
  depend on the derived data. (#55)

- `nm_write()` now outputs the source script in the meta data folder. (#55)

- `read_src_dir()` added a `.read_domains` argument to allow users to only read
  in specific domains. If not specified, default is to read all in. (#55)

- `read_src_dir()` added a `.subject_col` argument to allow users to specify the
  name of the unique subject identifier column in the source data. (#55)

- Added `view_src_dir_summary()` to allow the user to view all the domains and file
  sizes from their source data directory. (#55)

# mrgda 0.3.2

## Bug fixes

- `nm_write()` now allows for special characters in the file name. (#46)

# mrgda 0.3.1

## Bug fixes

- `read_src_dir()` now allows for source directories in which all files have no USUBJID. (#42)

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

