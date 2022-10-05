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

