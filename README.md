# mrgda

![mrgda logo](man/figures/logo.png)

mrgda is a small set of R helpers for assembling and inspecting clinical-style source data and derived datasets.

Examples below are illustrative; the outputs are mocked to show what the console would print.

## read_src_dir()

Read a source folder of SDTM or ADaM domains into a named list with labels and metadata.

```r
src <- read_src_dir("data/source/100", .file_types = "xpt")
names(src)
```

```text
read_src_dir Summary
Number of domains successfully loaded: 4
Number of domains that failed to load: 0
[1] "dm" "ex" "pc" "vs" "mrgda_labels" "mrgda_src_meta"
```

## summarize_src_list()

Summarize each domain with row and column counts, subjects, and date ranges.

```r
summarize_src_list(src)
```

```text
# A tibble: 3 x 7
  Domain  Rows Cols Subjects `Rows/Subj (ratio)` Date Min   Date Max   Date Col
  dm      100  20   100      1.0                 2024-01-01 2024-01-01 RFSTDTC
  ex      300  15   100      3.0                 2024-01-01 2024-02-01 EXSTDTC
  pc      800  18   100      8.0                 2024-01-01 2024-02-10 PCDTC
```

## compare_src_lists()

Compare two source lists and flag domains as identical, modified, added, or removed.

```r
compare_src_lists(src_v1, src_v2) %>% dplyr::select(Domain, Status, Rows)
```

```text
# A tibble: 3 x 3
  Domain Status    Rows
  dm     identical identical
  ex     modified  300 -> 320
  lb     added     added
```

## query_src_list()

Search across column names, labels, and values to find where a concept lives.

```r
query_src_list(src, "RACE")
```

```text
# A tibble: 2 x 4
  DOMAIN COLUMN MATCH_TYPE VALUE
  dm     RACE   column     NA
  dm     RACE   label      Race
```

## assign_id()

Add a stable numeric ID column from a subject identifier.

```r
df <- data.frame(USUBJID = c("01", "02", "01"), VISIT = c(1, 1, 2))
with_id <- assign_id(df, .subject_col = "USUBJID")
with_id[, c("USUBJID", "VISIT", "ID")]
```

```text
ID Summary
Number of subjects detected and assigned IDs: 2
  USUBJID VISIT ID
1 01      1     1
2 02      1     2
3 01      2     1
```

## read_csv_dots()

Read a NONMEM-style CSV where "." means missing.

```r
read_csv_dots("data/derived/pk.csv")[1:3, c("ID", "TIME", "DV")]
```

```text
  ID TIME   DV
1 1  0     12.3
2 1  1     NA
3 1  2     10.8
```

## write_csv_dots()

Write a CSV where missing values are encoded as ".".

```r
write_csv_dots(with_id, "data/derived/with_id.csv")
```

```text
Wrote 3 rows to data/derived/with_id.csv
```

## execute_data_diffs()

Generate a diff summary between two data frames and (optionally) per-subject diffs.

```r
diffs <- execute_data_diffs(base_df, compare_df, .subject_col = "ID")
diffs$diffs
```

```text
name            value
N Rows Diff     2 row(s) added
New Columns     DOSE
Compare data from local
N IDs Diff      1 ID(s) added
```

## write_derived()

Write a derived CSV plus a metadata folder with specs, diffs, and history.

```r
write_derived(pk, spec, "data/derived/pk.csv", .comment = "initial export")
```

```text
File written: /.../data/derived/pk.csv
Metadata folder: /.../data/derived/pk
```

## explain()

Print a quick explanation header and the result of a diagnostic expression.

```r
explain("Check duplicate time stamps", {
  dplyr::count(df, ID, TIME) %>% dplyr::filter(n > 1)
})
```

```text
Explanation: Check duplicate time stamps

# A tibble: 0 x 3
  ID TIME n
```

## `%>%` (pipe)

Re-exported magrittr pipe for readable data transforms.

```r
1:5 %>% sum()
```

```text
[1] 15
```
