rm(list=ls())
gc()
rstudioapi::restartSession()

devtools::document()
devtools::check()
.path <- devtools::build()
rcmdcheck::rcmdcheck(.path)
# pkgdown::build_site()
# pkgdown::deploy_to_branch()
