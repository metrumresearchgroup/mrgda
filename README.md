# mrgda – Data Assembly Workflow

- [Background](#background)
- [1 Source data](#1-source-data)
  - [Key Function: `read_src_dir()`](#key-function-read_src_dir)
- [2 Subject-level domains](#2-subject-level-domains)
  - [Demographics](#demographics)
  - [Baseline weight / BMI](#baseline-weight--bmi)
- [3 Time-varying domains](#3-time-varying-domains)
  - [PK concentrations](#pk-concentrations)
  - [Dosing events](#dosing-events)
  - [Time-varying body-weights
    (optional)](#time-varying-body-weights-optional)
- [4 Combine & derive analysis
  dataset](#4-combine--derive-analysis-dataset)
  - [Combine domains](#combine-domains)
  - [Carry baseline weight forward & drop placeholder
    rows](#carry-baseline-weight-forward--drop-placeholder-rows)
  - [NONMEM-specific derived columns](#nonmem-specific-derived-columns)
  - [Carry forward dose amounts](#carry-forward-dose-amounts)
  - [4.1 Assign subject IDs](#41-assign-subject-ids)
  - [Key Function: `assign_id()`](#key-function-assign_id)
- [5 Final checks & export](#5-final-checks--export)
  - [Conform to yspec-defined column
    order](#conform-to-yspec-defined-column-order)
  - [Validation rule: no specification
    violations](#validation-rule-no-specification-violations)
  - [Write to package-friendly
    location](#write-to-package-friendly-location)
  - [Key Function: `write_derived()`](#key-function-write_derived)

<img src="man/figures/logo.png" width="150px" />

# Background

This workflow utilizes several key functions from the `mrgda` package to
streamline the data assembly process. We’ll highlight three of them:

- `read_src_dir()`: Used for ingesting source data.
- `assign_id()`: For assigning consistent subject identifiers.
- `write_derived()`: For exporting the final analysis dataset along with
  its metadata.

# 1 Source data

## Key Function: `read_src_dir()`

The `read_src_dir()` function is a crucial first step in the data
assembly pipeline. It is designed to read all data files (domains) from
a specified source data directory, such as an SDTM or ADaM folder.

**Key Features:**

- **Path Specification**: You provide the full path to the source data
  directory using the `.path` argument.
- **File Type Selection**: You must specify the file type (e.g., CSV,
  SAS7BDAT, XPT) via `.file_types`.
- **Selective Domain Loading**: If you only need specific domains, you
  can provide a character vector of domain names to the `.read_domains`
  argument (e.g., `c('dm', 'lb')`). By default, it loads all domains.
- **Output**: It returns a named list of tibbles, where each tibble
  corresponds to an SDTM domain. This list also includes helper elements
  like `mrgda_labels` (for data labels) and `mrgda_src_meta` (containing
  metadata such as MD5 checksums, file types, and the source path).
- **Progress & Summary**: The function provides progress messages as it
  reads files and prints a summary of how many domains were successfully
  loaded versus how many failed.

In this workflow, `read_src_dir()` is used to load the SDTM source data.

``` r
# Prepare storage list for subject-level (sl) and time-varying (tv) data
out <- list(sl = list(), tv = list())

# Load PK specification file
target_spec <- ys_load(here("data/derived/pk.yaml"))

# Read SDTM source data from the '100' subdirectory
src_100 <- read_src_dir(here("data", "source", "100"), .file_types = "xpt")
```

# 2 Subject-level domains

## Demographics

First, we create a base dataframe from the DM domain for further
processing.

``` r
# Source data for demographics processing.
# This dataframe will be used as the base for deriving SEX, RACE, and base demographic components.
dm_for_processing <- src_100$dm %>% filter(ACTARM != "NOT ASSIGNED")
```

Next, we process the `SEX` variable.

``` r
# Process SEX variable, mapping character codes to numeric values.
# This creates a tibble with USUBJID and the processed SEX, stored in out$sl$sex.
out$sl$sex <-
  dm_for_processing %>%
  mutate(
    USUBJID,
    SEX = case_when(
      SEX == "F" ~ 1, # Female to 1
      SEX == "M" ~ 2, # Male to 2
      TRUE ~ -99 # Other/Missing to -99
    ),
    .keep = "none"
  )
```

Then, we process the `RACE` variable.

``` r
# Process RACE variable, mapping character codes to numeric values.
# This creates a tibble with USUBJID and the processed RACE, stored in out$sl$race.
out$sl$race <-
  dm_for_processing %>%
  mutate(
    USUBJID,
    RACE = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 3,
      RACE == "OTHER" ~ 6,
      TRUE ~ -99 # Other/Missing to -99
    ),
    .keep = "none"
  )
```

Finally, we create the age-related demographic information (STUDYID,
BLAGE).

``` r
# Create the age-related demographic information table, including USUBJID, STUDYID,
# and BLAGE (derived from AGE). This is stored in out$sl$age.
out$sl$age <-
  dm_for_processing %>%
  mutate(
    USUBJID,
    STUDYID,
    BLAGE = AGE, # Assign AGE to BLAGE
    .keep = "none"
  )
# Former assertion on a combined 'dm' table is removed as 'dm' is now assembled by 'reduce'.
# Specific assertions for SEX and RACE could be added in their respective processing chunks if needed.
```

## Baseline weight / BMI

This chunk processes the Vital Signs (VS) data to derive baseline weight
and BMI.

``` r
# Step 1: Filter VS data for relevant baseline records (WEIGHT and HEIGHT)
# for subjects present in the demographic processing data.
vs_1 <-
  src_100$vs %>%
  filter(
    USUBJID %in% dm_for_processing$USUBJID, # Subjects must be in the demographic data
    VSBLFL == "Y",                         # Baseline flag must be 'Y'
    VSTESTCD %in% c("WEIGHT", "HEIGHT")    # Only include WEIGHT and HEIGHT tests
  )

# Step 2: Select only the necessary columns: subject ID, test code,
# and standard result in numeric format.
vs_2 <-
  vs_1 %>%
  select(USUBJID, VSTESTCD, VSSTRESN)

# Step 3: Pivot the data from a long format to a wide format,
# so that WEIGHT and HEIGHT become separate columns.
vs_3 <-
  vs_2 %>%
  pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN)

# Step 4: Calculate Baseline Weight (BLWT) and Baseline BMI (BLBMI),
# and assign the result to out$sl$vs.
out$sl$vs <-
  vs_3 %>%
  mutate(
    USUBJID,
    BLWT  = WEIGHT, # Baseline Weight
    BLBMI = WEIGHT / ((HEIGHT / 100)^2), # Baseline BMI (Height in meters)
    .keep = "none"
  )

# Assertions:
# 1. Check for duplicate USUBJIDs in the final baseline vital signs data.
# 2. Check for any NA values in the final baseline vital signs data.
assert_that(!any(duplicated(out$sl$vs$USUBJID)), !anyNA(out$sl$vs))
```

# 3 Time-varying domains

## PK concentrations

This chunk filters and transforms PK concentration data.

``` r
# Filter PK data (pc) for relevant subjects and tests, and exclude "NOT DONE" records.
pk_conc_01 <-
  src_100$pc %>%
  filter(
    USUBJID %in% dm_for_processing$USUBJID, # Subject must be in demographics
    PCTEST == "DRUG-X",                     # Specific drug test
    PCSTAT != "NOT DONE"                    # Status is not "NOT DONE"
  )

# Mutate to create analysis variables: DV, BLQ, EVID, MDV, DATETIME, LLOQ.
out$tv$pc <-
  pk_conc_01 %>%
  mutate(
    USUBJID,
    DV       = PCSTRESN, # Dependent variable (concentration)
    BLQ      = if_else(PCORRES == "<LLOQ", 1, 0), # Below LLOQ flag
    EVID     = if_else(BLQ == 1, 2, 0), # Event ID (2 for BLQ observation, 0 otherwise)
    MDV      = if_else(is.na(DV), 1, 0), # Missing DV flag
    DATETIME = ymd_hms(PCDTC), # Standardized date-time
    LLOQ     = PCLLOQ, # Lower Limit of Quantification
    .keep = "none"
  )

# Assertion: Check for duplicate records based on USUBJID and DATETIME.
assert_that(!any(duplicated(out$tv$pc[c("USUBJID", "DATETIME")])))
```

## Dosing events

This chunk filters and transforms dosing event data from the EX domain.

``` r
# Filter dosing data (ex) for relevant subjects.
ex_filtered <-
  src_100$ex %>%
  filter(USUBJID %in% dm_for_processing$USUBJID) # Subject must be in demographics

# Mutate to create analysis variables: AMT, EVID, DATETIME, MDV.
out$tv$dosing <-
  ex_filtered %>%
  mutate(
    USUBJID,
    AMT      = EXDOSE,    # Dose amount
    EVID     = 1,         # Event ID (1 for dosing event)
    DATETIME = ymd_hms(EXSTDTC), # Standardized date-time of dosing
    MDV      = 1,          # MDV is 1 for dosing records
    .keep = "none"
  )

# Assertion: Check for any placeholder -99 values.
assert_that(!any(out$tv$dosing == -99, na.rm = TRUE))
```

## Time-varying body-weights (optional)

This chunk extracts time-varying body weight measurements from the VS
domain.

``` r
# Filter VS data for "WEIGHT" tests to get time-varying weights.
vs_weight_records <-
  src_100$vs %>%
  filter(VSTEST == "WEIGHT")

# Mutate to create analysis variables: WT, DATETIME, EVID (placeholder).
out$tv$wt <-
  vs_weight_records %>%
  mutate(
    USUBJID,
    WT       = VSSTRESN, # Body weight
    DATETIME = ymd_hms(VSDTC), # Standardized date-time of measurement
    EVID     = -99, # Placeholder EVID, to be handled later
    .keep = "none"
  )
```

# 4 Combine & derive analysis dataset

## Combine domains

This chunk combines the subject-level and time-varying data into a
single dataset. \# The `out$sl` list now contains `age`, `sex`, `race`,
and `vs`. \# The `reduce` function will join these tables by `USUBJID`.

``` r
# Step 1: Combine all time-varying datasets (PK, dosing, time-varying weights)
# from the 'out$tv' list by stacking them vertically.
tv_combined_data <- bind_rows(out$tv)

# Step 2: Combine all subject-level datasets (age, sex, race, baseline VS)
# from the 'out$sl' list by iteratively joining them by USUBJID.
sl_combined_data <- reduce(out$sl, full_join, by = "USUBJID")

# Step 3: Join the combined time-varying data with the combined subject-level data.
# This uses a left_join, keeping all rows from tv_combined_data.
pk_joined_intermediate <- left_join(tv_combined_data, sl_combined_data, by = "USUBJID")

# Step 4: Arrange the resulting dataset by USUBJID and then by DATETIME
# for chronological order within each subject.
pk_00 <- arrange(pk_joined_intermediate, USUBJID, DATETIME)
```

## Carry baseline weight forward & drop placeholder rows

This chunk processes baseline weight and removes placeholder rows for
time-varying weights.

``` r
# Step 1: Group by USUBJID and carry forward (and backward using "downup")
# the WT (weight) column. This populates missing weights with the last known value.
pk_01_filled <-
  pk_00 %>%
  group_by(USUBJID) %>%
  fill(WT, .direction = "downup") %>%
  ungroup()

# Step 2: Filter out rows that were placeholders for time-varying weights (EVID == -99).
pk_01 <-
  pk_01_filled %>%
  filter(EVID != -99)
```

## NONMEM-specific derived columns

This chunk derives columns commonly used in NONMEM modeling.

``` r
# Step 1: Calculate FIRST_DOSE_TIME, N_DOSES, and TIME.
# - FIRST_DOSE_TIME: The earliest DATETIME where EVID is 1 (a dose).
# - N_DOSES: Cumulative count of dosing events (EVID == 1), NA replaced with 0.
# - TIME: Time in hours since the FIRST_DOSE_TIME.
pk_02_timecalc <-
  pk_01 %>%
  group_by(USUBJID) %>%
  mutate(
    FIRST_DOSE_TIME = min(DATETIME[EVID == 1], na.rm = TRUE),
    N_DOSES         = replace_na(cumsum(EVID == 1), 0),
    TIME            = as.numeric(difftime(DATETIME, FIRST_DOSE_TIME, units = "hours"))
  ) %>%
  ungroup()

# Step 2: Calculate TAD (Time After Dose) and DOSE_HAD_PK.
# These are calculated per subject and per dose number (N_DOSES).
# - TAD: Time in hours since the start of the current dose.
# - DOSE_HAD_PK: A flag indicating if any PK sample (EVID == 0) exists for the current dose.
pk_02_tadcalc <-
  pk_02_timecalc %>%
  group_by(USUBJID, N_DOSES) %>%
  mutate(
    TAD           = as.numeric(difftime(DATETIME, min(DATETIME), units = "hours")),
    DOSE_HAD_PK   = any(EVID == 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Calculate NEW_DOSE and OCC (Occasion).
# - NEW_DOSE: A flag indicating if the current N_DOSES is different from the previous row's.
# - OCC: An occasion number, incrementing each time a NEW_DOSE occurs AND that dose
#        interval had a PK sample (DOSE_HAD_PK). It's 0 if N_DOSES is 0.
pk_02_occcalc <-
  pk_02_tadcalc %>%
  group_by(USUBJID) %>%
  mutate(
    NEW_DOSE = N_DOSES != lag(N_DOSES, default = first(N_DOSES)),
    OCC      = cumsum(NEW_DOSE & DOSE_HAD_PK) * (N_DOSES != 0)
  ) %>%
  ungroup()

# Assign the final prepared dataset for this stage
pk_02 <- pk_02_occcalc
```

## Carry forward dose amounts

This chunk ensures that dose amounts (AMT) are carried forward to
observation records.

``` r
# Step 1: Initialize a DOSE column with the AMT (dose amount).
# AMT is typically present only on dosing records (EVID == 1).
pk_03_amtinit <-
  pk_02 %>%
  mutate(DOSE = AMT)

# Step 2: Group by USUBJID and carry forward (and backward using "downup") the DOSE amount.
# This populates the DOSE column for observation records based on the last known dose.
pk_03 <-
  pk_03_amtinit %>%
  group_by(USUBJID) %>%
  fill(DOSE, .direction = "downup") %>%
  ungroup()
```

## 4.1 Assign subject IDs

## Key Function: `assign_id()`

The `assign_id()` function is designed to create a new column in your
dataset, typically named `ID`, which will contain unique numerical
identifiers for each individual subject.

Here’s a breakdown of its core functionality:

1.  **Identifying Subjects**: The function first needs to know which
    column in your current dataset already holds the unique subject
    identifiers (e.g., `USUBJID`). You specify this using the
    `.subject_col` argument.

2.  **Leveraging Previous Identifiers (for consistency)**:

    - You can provide the path to a *previously derived version* of this
      dataset using the `.previously_derived_path` argument.
    - For any subjects found in both datasets, `assign_id()` ensures
      they receive the *exact same numerical `ID`* they had in the
      previous version. This is vital for maintaining consistency and
      traceability of subjects across different iterations of your
      analysis dataset.

3.  **Assigning New Identifiers**:

    - If a subject in your current dataset is new (i.e., they were not
      present in the `.previously_derived_path` dataset, or if no
      previous dataset path was given), `assign_id()` will assign them a
      brand new, unique numerical `ID`.

4.  **Outputting the `ID` Column**: After determining the appropriate
    numerical ID for every subject, the function adds a new column named
    `ID` (by default) to your dataset, populating it with these unique
    numerical identifiers.

``` r
# Adds an integer ID column using the assign_id function.
pk_04 <- assign_id(pk_03, .subject_col = "USUBJID")
```

# 5 Final checks & export

## Conform to yspec-defined column order

This chunk adds final columns (NUM, C) and reorders all columns
according to the target specification.

``` r
# Add a row number (NUM) and a placeholder comment column (C).
# Then, select columns in the order defined by the 'target_spec' object.
pk_final <-
  pk_04 %>%
  mutate(
    NUM = row_number(),
    C   = NA_character_
  ) %>%
  select(names(target_spec))
```

## Validation rule: no specification violations

This chunk validates the final dataset (`pk_final`) against the
`target_spec`.

``` r
ys_check(pk_final, target_spec)
```

## Write to package-friendly location

## Key Function: `write_derived()`

The `write_derived()` function is responsible for exporting your final,
processed dataset and creating a comprehensive set of associated
metadata. This promotes reproducibility and makes it easier to track
dataset versions and understand their contents.

**Key Features:**

- **Input Data & Specification**:
  - `.data`: The R data frame to be written.
  - `.spec`: A `yspec` object that defines the data specifications
    (column names, types, labels, etc.). The function checks the data
    against this specification before writing.
  - `.file`: The path and filename for the output CSV file.
- **Metadata Creation**: A key feature is the creation of a metadata
  folder (named after the output file, without the extension, and
  located in the same directory). This folder contains:
  - **XPT file**: An FDA-style XPT version of the dataset (using
    `haven::write_xpt`).
  - **`spec-list.yml`**: A YAML file containing the data specification
    (derived from the `yspec` object).
  - **Define-XML**: Rendered HTML and PDF versions of the data
    definition (using `yspec::render_fda_define`).
  - **`subject-columns.yml`**: A YAML file identifying subject-level
    columns.
  - **`history.csv`**: Tracks changes to the dataset across versions,
    including user, date/time, comments, and revision numbers (if
    applicable).
  - **`sys-info.yml`**: System information at the time of writing (user,
    R version, OS).
  - **`dependencies.yml`**: If run within an RStudio project, it
    attempts to find scripts that use this derived file.
  - **`diffs.csv` & `subject-diffs.csv`**: If a previous version of the
    dataset is available (either locally or from SVN, controlled by
    `.compare_from_svn`), it generates diff reports highlighting changes
    between versions.
- **Versioning & Comparison**:
  - `.prev_file`: Path to the previous version of the CSV file (defaults
    to `.file` if not specified).
  - `.compare_from_svn`: Logical; if `TRUE`, it compares against the
    latest SVN version of `.prev_file`.
- **Output**: Writes the CSV file and creates the metadata folder. It
  also prints messages indicating the locations of the written file and
  metadata folder.
- **Return Value**: By default, it returns `NULL` invisibly. If
  `.return_base_compare` is `TRUE`, it returns a list containing the
  current and previous versions of the datasets used for comparison.

This function ensures that derived datasets are not just saved as simple
CSVs but are accompanied by rich metadata that is essential for
regulatory submissions, collaboration, and long-term project
maintenance.

``` r
# Write the final PK dataset and associated metadata.
write_derived(pk_final, target_spec, here("data/derived/pk.csv"))

# Display session info for reproducibility.
sessionInfo()
```
