#' View SDTM Domain Abbreviations
#'
#' @description
#' Helper function to view typical SDTM domains. Both the abbreviation and domain
#' description is provided.
#'
#' @examples
#' view_sdtm_domains()
#'
#' @export
view_sdtm_domains <- function(){

    system.file("package-data", "sdtm-domains.csv", package = "mrgda") %>%
    readr::read_csv(file = .) %>%
    suppressMessages()
}
