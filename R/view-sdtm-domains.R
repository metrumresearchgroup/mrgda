#' View SDTM Domain Abbreviations
#'
#' @export
view_sdtm_domains <- function(){

  system.file("view", "sdtm-domains.csv", package = "mrgda") %>%
  readr::read_csv(file = .) %>%
  View("SDTM Domains")

}
