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

