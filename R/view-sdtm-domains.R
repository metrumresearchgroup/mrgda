#' View SDTM Domain Abbreviations
#'
#' @export
view_sdtm_domains <- function(){

  system.file("package-data", "sdtm-domains.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    utils::View("SDTM Domains")

}
