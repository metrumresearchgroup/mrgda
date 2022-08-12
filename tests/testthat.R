library(testthat)
library(nmvalidate)

nm_spec <- yspec::ys_load(
  system.file("derived", "pk.yml", package = "nmvalidate")
)
nm <- readr::read_csv(
  system.file("derived", "pk.csv", package = "nmvalidate"),
  na = "."
)
nm_errors <- readr::read_csv(
  system.file("derived", "pk-errors.csv", package = "nmvalidate"),
  na = "."
)

res_nm <- nm_validate(nm, nm_spec)
res_nm_errors <- nm_validate(nm_errors, nm_spec)
