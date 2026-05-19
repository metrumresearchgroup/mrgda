# CDISCPILOT02 Source Data

`CDISCPILOT02` is a deliberately imperfect source-data package for validation
and data assembly examples. It started as a copy of the clean `CDISCPILOT01`
example source package, then was modified to add a small, traceable set of
common SDTM/source-data issues.

## What Was Done

- The `CDISCPILOT01` source package was copied to `CDISCPILOT02`.
- All XPT domains were rewritten with `STUDYID = "CDISCPILOT02"`.
- Supporting non-XPT files from `CDISCPILOT01` were copied into this directory.
- The XPT files were modified with the intentional defects listed below.
- `defect-manifest.csv` was written with the same defect list in machine-readable form.

## Intentional Issues

| Domain | Record(s) | Issue |
| --- | --- | --- |
| ALL | all XPT domains | `STUDYID` changed from `CDISCPILOT01` to `CDISCPILOT02`. |
| EX | `01-701-1028`, `EXSEQ = 1` | `EXSTDTC` is date-only (`2013-07-19`) while most exposure start values are full datetimes. |
| EX | `01-701-1034`, `EXSEQ = 2` | `EXENDTC` is date-only (`2014-12-17`) while most exposure end values are full datetimes. |
| EX | `01-701-1097`, `EXSEQ = 1` | Both `EXSTDTC` and `EXENDTC` are date-only. |
| EX | `01-701-1028`, `EXSEQ = 1` and `2` | Exposure intervals overlap on `2013-08-01`. |
| EX | `01-701-1034`, `EXSEQ = 1` and `2` | Exposure intervals have a one-day gap on `2014-07-16`. |
| PC | `01-701-1028`, baseline plasma XAN, `PCSEQ = 4` and `5` | Duplicate actual collection datetime (`PCDTC = 2013-07-19T01:00:00`) with unique `PCSEQ` and distinct nominal times (`PCTPTNUM = 1` and `1.5`). |
| PC | `01-701-1028`, baseline plasma XAN, `PCSEQ = 10` | `PCDTC` is blank while nominal time remains `12h Post-dose`. |
| PC | `01-701-1034`, baseline plasma XAN, `PCSEQ = 7` | `PCDTC` is partial with hour precision only (`2014-07-01T04`). |
| PC | `01-701-1034`, baseline plasma XAN, `PCSEQ = 2` | `PCORRES` and `PCSTRESC` are `<BLQ`, while `PCSTRESN` remains numeric. |
| VS | `01-701-1015`, weight `VSSEQ = 142` and `143` | Two weight records are flagged as baseline. |
| AE | `01-701-1015`, `AESEQ = 3` | `AEENDTC` (`2014-01-08`) is before `AESTDTC` (`2014-01-09`). |
| LB | `01-701-1015`, `LBSEQ = 1`, `LBTESTCD = ALB` | `LBSTRESN` is missing while `LBSTRESC` remains populated. |
| DM | `01-701-1028` | `RFXSTDTC` is missing for a treated subject. |
| SV | `01-701-1028`, `VISITNUM = 4` | `SVENDTC` (`2013-07-31`) is before `SVSTDTC` (`2013-08-01`). |

## Verification Notes

- Most populated `EXSTDTC` and `EXENDTC` values are full ISO datetime strings.
- Only selected EX rows have date-only values to represent missing time components.
- `PCSEQ` remains unique within each subject.
- The intentional duplicate PK timing issue is by `USUBJID`, `PCTESTCD`, `PCSPEC`, and `PCDTC`, not by `PCSEQ`.
- All XPT files were written with `haven::write_xpt()` and can be read with `haven::read_xpt()`.

## Files

- `*.xpt`: rewritten SDTM-like source domains.
- `defect-manifest.csv`: machine-readable list of intentional issues.
- `README.md`: this human-readable summary.
