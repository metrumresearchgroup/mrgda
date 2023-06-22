#' @keywords internal
get_sdtm_lookup <- function(.domain_name) {
  lookup <- yaml::read_yaml(system.file("package-data/sdtm-lookup.yaml", package = "mrgda"))
  lookup[[.domain_name]]
}


