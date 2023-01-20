#' View SDTM Domain Abbreviations
#'
#' @import utils
#' @export
view_sdtm_domains <- function(){

  system.file("package-data", "sdtm-domains.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    View("SDTM-Domains")

}
