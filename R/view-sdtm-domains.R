#' View SDTM Domain Abbreviations
#'
#' @param .view Logical. Should the output be viewed?
#'
#' @export
view_sdtm_domains <- function(.view = TRUE){

  .sdtm_domains <-
    system.file("package-data", "sdtm-domains.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    suppressMessages()

  if (.view) {
    tibble::view(.sdtm_domains, "SDTM Domains")
  } else {
    return(.sdtm_domains)
  }

}
