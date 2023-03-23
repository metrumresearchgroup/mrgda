# mrgda 0.5.1

## New features and changes

- Added `view_mrgda_flags()` to allow the user to view a summary of how mrgda flags in the specification file are assigned to the data set. (#67)

- `read_src_dir()` now outputs the name and size of the current file its reading in. (#67)

- `query_src_dir()` now allows the user to specify file types. (#67)

## Bug fixes

- `nm_summary()` now will only run if all required flags are available. (#67)

- `nm_validate()` has stricter requirements for the data it needs defined to run each check. (#67)

- `read_src_dir()` Allows non-detect method for file type discovery. (#67)
