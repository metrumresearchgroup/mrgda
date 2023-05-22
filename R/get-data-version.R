#' Get data version
#'
#' This function reads a YAML file containing version information about data and
#' returns the latest version along with the corresponding file name.
#'
#' @title Get Data Version
#'
#' @description Reads the latest version of data from a YAML file and
#' constructs the filename for that data version.
#'
#' @param .endpoint A character string denoting the endpoint to be used for file naming.
#' @param .lookup_path The path to the YAML file containing version information.
#'
#' @return A list containing the information of the latest version and
#' the corresponding file name.
#'
#' @examples
#' # Make sure the yaml file exists at the specified path before running
#' get_data_version(
#'   .endpoint = "pk",
#'   .lookup_path = system.file("derived/data-version-lookup.yaml", package = "mrgda")
#'   )
#'
#' @export
get_data_version <- function(.endpoint, .lookup_path){

  out <- list()

  out$info <-
    yaml::read_yaml(.lookup_path) %>%
    dplyr::bind_rows() %>%
    suppressMessages() %>%
    dplyr::arrange(-Major, -Minor) %>%
    dplyr::slice(1)

  out$file <-
    out$info %>%
    dplyr::mutate(File = glue::glue("{.endpoint}-{Major}-{Minor}")) %>%
    dplyr::pull(File) %>%
    as.character()

  return(out)
}
