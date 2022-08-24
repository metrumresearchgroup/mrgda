library(dplyr)

nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "nmvalidate"))
nm <- readr::read_csv(system.file("derived", "pk.csv", package = "nmvalidate"), na = ".", show_col_types = FALSE)
nm_errors <- readr::read_csv(system.file("derived", "pk-errors.csv", package = "nmvalidate"), na = ".", show_col_types = FALSE)

