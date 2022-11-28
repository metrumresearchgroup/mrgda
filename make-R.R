rm(list=ls())
gc()
rstudioapi::restartSession()

devtools::document()
devtools::check()
.path <- devtools::build()
rcmdcheck::rcmdcheck(.path)
