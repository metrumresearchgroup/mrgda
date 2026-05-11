# CDISCPILOT01 Source Data

These dummy source datasets were copied from the `pharmaversesdtm` R package
and written as XPT files for the installed example project.

```r
dir.create(
  "inst/example-project/data/source/CDISCPILOT01",
  recursive = TRUE,
  showWarnings = FALSE
)

haven::write_xpt(pharmaversesdtm::dm, "inst/example-project/data/source/CDISCPILOT01/dm.xpt")
haven::write_xpt(pharmaversesdtm::ex, "inst/example-project/data/source/CDISCPILOT01/ex.xpt")
haven::write_xpt(pharmaversesdtm::lb, "inst/example-project/data/source/CDISCPILOT01/lb.xpt")
haven::write_xpt(pharmaversesdtm::pc, "inst/example-project/data/source/CDISCPILOT01/pc.xpt")
haven::write_xpt(pharmaversesdtm::vs, "inst/example-project/data/source/CDISCPILOT01/vs.xpt")
```
